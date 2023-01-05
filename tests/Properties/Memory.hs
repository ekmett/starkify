{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Properties.Memory where

import Properties.Common
import Data.Word
import qualified Language.Wasm.Structure as W
import GHC.Natural
import Test.QuickCheck hiding ((.&.))
import Data.Proxy
import Data.Int
import qualified Eval as Eval
import qualified MASM.Miden as Miden
import qualified Data.Text.Lazy.IO as T
import W2M (toMASM)
import Text.Pretty.Simple (pShow)
import Validation (runValidation)
import Control.Monad
import Data.Bits
import System.IO.Temp

data MemAddr = MemAddr
  { addrBase   :: Word32
  , addrOffset :: Word32
  }

memAddr :: MemAddr -> Word32
memAddr (MemAddr a b) = a + b

newtype MemAlign = MemAlign { memAlign :: Natural }

data MemStore mem a = MemStore
  { msAddr  :: MemAddr   -- ^ target address
  , msAlign :: MemAlign  -- ^ alignment
  , msVal   :: a         -- ^ value to store (potentially smaller than memory cells)
  , msPrev  :: mem       -- ^ previous value of whole memory cell
  }

data MemLoad mem a = MemLoad
  { mlAddr  :: MemAddr  -- ^ target address
  , mlAlign :: MemAlign -- ^ alignment
  , mlVal   :: mem      -- ^ what (potentially larger than @a@) value to put in cell
  }

instance MemCellStore mem val => Show (MemStore mem val) where
  show = showStore

instance MemCellLoad mem val => Show (MemLoad mem val) where
  show = showLoad

toMemArg :: MemStore mem a -> W.MemArg
toMemArg (MemStore addr align _a _prevVal) =
  W.MemArg (fromIntegral $ addrOffset addr) (memAlign align)

toMemArgL :: MemLoad mem a -> W.MemArg
toMemArgL (MemLoad addr align _a) =
  W.MemArg (fromIntegral $ addrOffset addr) (memAlign align)

instance (MemCellStore mem a, Arbitrary a, Arbitrary mem) => Arbitrary (MemStore mem a) where
  arbitrary = MemStore <$> genAddr
                       <*> (fmap (MemAlign . fromIntegral) (genAligns (Proxy @mem) (Proxy @a)))
                       <*> arbitrary
                       <*> arbitrary
    where addrMultiple = case cellTy (Proxy @mem) of
            W32 -> 4
            W64 -> 8
            _ -> error "shouldn't happen"
          genAddr = MemAddr <$> genBaseAddr <*> genOffset
          genBaseAddr = (addrMultiple*) <$> choose (0, 10)
          genOffset = genOffsets (Proxy @mem) (Proxy @a)

instance (MemCellStore mem a, Arbitrary mem) => Arbitrary (MemLoad mem a) where
  arbitrary = MemLoad <$> genAddr
                      <*> (fmap (MemAlign . fromIntegral) (genAligns (Proxy @mem) (Proxy @a)))
                      <*> arbitrary
    where addrMultiple = case cellTy (Proxy @mem) of
            W32 -> 4
            W64 -> 8
            _ -> error "shouldn't happen"
          genAddr = MemAddr <$> genBaseAddr <*> genOffset
          genBaseAddr = (addrMultiple*) <$> choose (0, 10)
          genOffset = genOffsets (Proxy @mem) (Proxy @a)

class MemCell mem where
  cellTy :: Proxy mem -> Ty
  wasmLoadRaw :: Proxy mem -> (W.MemArg -> W.Instruction Natural)
  wasmStoreRaw :: Proxy mem -> (W.MemArg -> W.Instruction Natural)

instance MemCell Word32 where
  cellTy _ = W32
  wasmLoadRaw _ = W.I32Load
  wasmStoreRaw _ = W.I32Store
instance MemCell Word64 where
  cellTy _ = W64
  wasmLoadRaw _ = W.I64Load
  wasmStoreRaw _ = W.I64Store

class MemCell mem => MemSubcell mem a where
  genAligns  :: Proxy mem -> Proxy a -> Gen Word8
  genOffsets :: Proxy mem -> Proxy a -> Gen Word32

class MemSubcell mem a => MemCellStore mem a where
  wasmStore :: Proxy mem -> Proxy a -> (W.MemArg -> W.Instruction Natural)
  checkStore :: MemStore mem a -> (mem -> Property)
  showStore :: MemStore mem a -> String

class MemSubcell mem a => MemCellLoad mem a where
  wasmLoad  :: Proxy mem -> Proxy a -> (W.MemArg -> W.Instruction Natural)
  checkLoad :: MemLoad mem a -> (mem -> Property)
  showLoad :: MemLoad mem a -> String

instance MemSubcell Word32 Word32 where
  genAligns _ _ = return 2
  genOffsets _ _ = return 0
instance MemCellStore Word32 Word32 where
  wasmStore _ _ = W.I32Store
  checkStore (MemStore _addr _align a _prev) = (a===)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i32.store of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word32 Word32 where
  wasmLoad _ _ = W.I32Load
  checkLoad (MemLoad _ _ a) = (a===)
  showLoad (MemLoad addr (MemAlign align) a) =
    "i32.load of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align

instance MemSubcell Word32 Word8 where
  genAligns _ _ = return 0
  genOffsets _ _ = elements [0..3]
instance MemCellStore Word32 Word8 where
  wasmStore _ _ = W.I32Store8
  checkStore (MemStore addr _align a prev) res = do
    let r = memAddr addr `mod` 4
        mask = complement (shiftL 255 (fromIntegral $ 8*r))
        prev_masked = prev .&. mask
        ashifted = shiftL (fromIntegral a) (fromIntegral $ 8*r)
        res' = prev_masked .|. ashifted
    extract8 res === a .&&. res === res'
    where extract8 :: Word32 -> Word8
          extract8 v = case addrOffset addr `mod` 4 of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xff (8*fromIntegral i)) (8*fromIntegral i)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i32.store8 of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word32 Word8 where
  wasmLoad _ _ = W.I32Load8U
  checkLoad (MemLoad addr _alig a) res = do
    let r8 = fromIntegral res :: Word8
    extract8 a === r8
    where extract8 :: Word32 -> Word8
          extract8 v = case addrOffset addr `mod` 4 of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xff (8*fromIntegral i)) (8*fromIntegral i)
  showLoad (MemLoad addr (MemAlign align) a) =
    "i32.load8_u of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align

instance MemSubcell Word32 Int8 where
  genAligns _ _ = return 0
  genOffsets _ _ = elements [0..3]
instance MemCellStore Word32 Int8 where
  wasmStore _ _ = W.I32Store8
  checkStore (MemStore addr _align a prev) res = do
    let r = memAddr addr `mod` 4
        mask = complement (shiftL 255 (fromIntegral $ 8*r))
        prev_masked = prev .&. mask
        ashifted = shiftL (fromIntegral a) (fromIntegral $ 8*r)
        res' = prev_masked .|. ashifted
    fromIntegral (extract8 res) === a .&&. res === res'
    where extract8 :: Word32 -> Word8
          extract8 v = case addrOffset addr `mod` 4 of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xff (8*fromIntegral i)) (8*fromIntegral i)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i32.store8 (int8) of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word32 Int8 where
  wasmLoad _ _ = W.I32Load8S
  checkLoad (MemLoad addr align a) res = checkLoad (MemLoad addr align a :: MemLoad Word32 Word8) res
  showLoad (MemLoad addr (MemAlign align) a) =
    "i32.load8_s of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align

instance MemSubcell Word32 Word16 where
  genAligns _ _ = return 1
  genOffsets _ _ = elements [0, 2]
instance MemCellStore Word32 Word16 where
  wasmStore _ _ = W.I32Store16
  checkStore (MemStore addr _align a prev) res = do
    let r = memAddr addr `mod` 4
        mask = complement (shiftL 65535 (fromIntegral $ 8*r))
        prev_masked = prev .&. mask
        ashifted = shiftL (fromIntegral a) (fromIntegral $ 8*r)
        res' = prev_masked .|. ashifted
    extract16 res === a .&&. res === res'
    where extract16 :: Word32 -> Word16
          extract16 v = case addrOffset addr of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xffff (8*fromIntegral i)) (8*fromIntegral i)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i32.store16 of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word32 Word16 where
  wasmLoad _ _ = W.I32Load16U
  checkLoad (MemLoad addr _align a) res = do
    let r16 = fromIntegral res :: Word16
    extract16 a === r16
    where extract16 :: Word32 -> Word16
          extract16 v = case addrOffset addr of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xffff (8*fromIntegral i)) (8*fromIntegral i)
  showLoad (MemLoad addr (MemAlign align) a) =
    "i32.load16_u of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align

instance MemSubcell Word64 Word64 where
  genAligns _ _ = return 3
  genOffsets _ _ = return 0
instance MemCellStore Word64 Word64 where
  wasmStore _ _ = W.I64Store
  checkStore (MemStore _addr _align a _prev) = (a===)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i64.store of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word64 Word64 where
  wasmLoad _ _ = W.I64Load
  checkLoad (MemLoad _addr _align a) = (a===)
  showLoad (MemLoad addr (MemAlign align) a) =
    "i64.load of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align

instance MemSubcell Word64 Word8 where
  genAligns _ _ = return 0
  genOffsets _ _ = elements [0..7]
instance MemCellStore Word64 Word8 where
  wasmStore _ _ = W.I64Store8
  checkStore (MemStore addr _align a prev) res = do
    let r = memAddr addr `mod` 8
        mask = complement (shiftL 255 (fromIntegral $ 8*r))
        prev_masked = prev .&. mask
        ashifted = shiftL (fromIntegral a) (fromIntegral $ 8*r)
        res' = prev_masked .|. ashifted
    extract8 res === a .&&. res === res'
    where extract8 :: Word64 -> Word8
          extract8 v = case addrOffset addr `mod` 8 of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xff (8*fromIntegral i)) (8*fromIntegral i)
  showStore (MemStore addr (MemAlign align) a prev) =
    "i64.store8 of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align ++
    " and previous value=" ++ show prev
instance MemCellLoad Word64 Word8 where
  wasmLoad _ _ = W.I64Load8U
  checkLoad (MemLoad addr _align a) res = do
    let res8 = fromIntegral res :: Word8
    extract8 a === res8
    where extract8 :: Word64 -> Word8
          extract8 v = case addrOffset addr `mod` 8 of
            i -> fromIntegral $ shiftR (v .&. shiftL 0xff (8*fromIntegral i)) (8*fromIntegral i)
  showLoad (MemLoad addr (MemAlign align) a) =
    "i64.load8_u of " ++ show a ++ " at address=" ++ show (addrBase addr) ++
    ", offset=" ++ show (addrOffset addr) ++
    " with align=" ++ show align


memStoreToWasm
  :: forall mem val.
     ( MemCellStore mem val
     , Num mem, Typed mem, Integral val
     )
  => MemStore mem val -> W.Module
memStoreToWasm s@(MemStore addr _align a prev) = W.Module
  { types = [W.FuncType [] [retty]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] instrs ]
  , mems = [ W.Memory (W.Limit 1 Nothing) ]
  , tables = [], globals = [], elems = [], datas = [], imports = []
  }

  where retty = case cellTy (Proxy @mem) of
          W32 -> W.I32
          I32 -> W.I32
          W64 -> W.I64
          I64 -> W.I64

        instrs =
          [ W.I32Const (addrBase addr)
          , wasmPush prev
          , wasmStoreRaw (Proxy @mem) (zeroOffset $ toMemArg s)
          , W.I32Const (addrBase addr)
          , wasmPush (fromIntegral a :: mem)
          , wasmStore (Proxy @mem) (Proxy @val) (toMemArg s)
          , W.I32Const (addrBase addr)
          , wasmLoadRaw (Proxy @mem) $ zeroOffset (toMemArg s)
          ]

        zeroOffset (W.MemArg _off alig) = W.MemArg 0 alig

memStoreTest
  :: forall mem a.
     ( MemCellLoad mem mem, MemCellStore mem a, Num mem
     , Typed mem, Show mem, Integral a
     )
  => Bool -> MemStore mem a -> Property
memStoreTest debug s = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "store: " ++ show s ++ " ===")
  let wmod = memStoreToWasm s
  Just reference <- fromWStack <$> Eval.simulateWASM wmod
  when debug $ putStrLn $ "result = " ++ show reference
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ T.putStrLn (pShow mmod)
    tmpfilename <- emptySystemTempFile "starkify-memstore-test-XXX.masm"
    let keep = if debug then Miden.Keep tmpfilename else Miden.DontKeep
    res <- Eval.runMiden keep mmod
    Miden.whenKeep keep $ \fp -> putStrLn ("MASM module saved at: " ++ fp)
    return res
  when debug $ print mres
  case mres of
    Left err -> error ("memStoreTest got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> mem -> Property
        check midenstack _val = case fromMStack midenstack of
          Just res -> checkStore s res
          Nothing -> error "couldn't extract result"

memLoadToWasm
  :: forall mem val.
     (MemCellLoad mem val, Typed mem)
  => MemLoad mem val -> W.Module
memLoadToWasm l@(MemLoad addr _align a) = W.Module
  { types = [W.FuncType [] [retty]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] instrs ]
  , mems = [ W.Memory (W.Limit 1 Nothing) ]
  , tables = [], globals = [], elems = [], datas = [], imports = []
  }

  where retty = case cellTy (Proxy @mem) of
          W32 -> W.I32
          I32 -> W.I32
          W64 -> W.I64
          I64 -> W.I64

        instrs =
          [ W.I32Const (addrBase addr)
          , wasmPush a
          , wasmStoreRaw (Proxy @mem) (zeroOffset $ toMemArgL l)
          , W.I32Const (addrBase addr)
          , wasmLoad (Proxy @mem) (Proxy @val) (toMemArgL l)
          ]

        zeroOffset (W.MemArg _off alig) = W.MemArg 0 alig

memLoadTest
  :: forall mem a.
     ( MemCellLoad mem a, Num mem
     , Typed mem, Show mem, Integral a, Eq mem
     )
  => Bool -> MemLoad mem a -> Property
memLoadTest debug l = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "load: " ++ show l ++ " ===")
  let wmod = memLoadToWasm l
  Just reference <- fromWStack <$> Eval.simulateWASM wmod
  when debug $ putStrLn $ "result = " ++ show reference
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ T.putStrLn (pShow mmod)
    tmpfilename <- emptySystemTempFile "starkify-memload-test-XXX.masm"
    let keep = if debug then Miden.Keep tmpfilename else Miden.DontKeep
    res <- Eval.runMiden keep mmod
    Miden.whenKeep keep $ \fp -> putStrLn ("MASM module saved at: " ++ fp)
    return res
  when debug $ print mres
  case mres of
    Left err -> error ("memLoadTest got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> mem -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> res === val .&&. checkLoad l res
          Nothing -> error "couldn't extract result"
