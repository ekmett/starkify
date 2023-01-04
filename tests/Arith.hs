{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Arith where

import Control.Monad ( when )
import Data.Bits ( Bits(..) )
import Data.Int ( Int32, Int64 )
import Data.Maybe (listToMaybe)
import Data.Word ( Word32, Word64 )
import GHC.Natural ( Natural )
import System.IO ( hSetBuffering, stdout, BufferMode(..) )
import Test.Hspec ( hspec, describe )
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck
    ( Gen,
      Property,
      (===),
      ioProperty,
      Arbitrary(arbitrary),
      chooseInt,
      frequency, NonZero (..), elements )

import qualified Language.Wasm.Interpreter as W
import qualified Language.Wasm.Structure as W

import qualified Eval as Miden
import Validation (runValidation)
import W2M (toMASM)
import MASM.Interpreter ( fromFakeW64, FakeW64(FakeW64) )

-- used for debugging info, see toplevel 'debug' value
import Text.Pretty.Simple ( pShow )
import qualified Data.Text.Lazy.IO as T

data Expr t where
    ConstU32 :: Word32           -> Expr Word32
    ConstI32 :: Int32            -> Expr Int32
    ConstU64 :: Word64           -> Expr Word64
    ConstI64 :: Int64            -> Expr Int64
    Add      :: Expr a -> Expr a -> Expr a
    Sub      :: Expr a -> Expr a -> Expr a
    Mul      :: Expr a -> Expr a -> Expr a
    Div      :: Expr a -> Expr a -> Expr a
    Shl      :: Expr a -> Int    -> Expr a
    Shr      :: Expr a -> Int    -> Expr a

instance Show (Expr t) where
  show e = case e of
    ConstU32 w -> show w
    ConstI32 w -> show w
    ConstU64 w -> show w
    ConstI64 w -> show w
    Add a b    -> "(" ++ show a ++ " + " ++ show b ++ ")"
    Sub a b    -> "(" ++ show a ++ " - " ++ show b ++ ")"
    Mul a b    -> "(" ++ show a ++ " * " ++ show b ++ ")"
    Div a b    -> "(" ++ show a ++ " / " ++ show b ++ ")"
    Shl a n    -> "(" ++ show a ++ " << " ++ show n ++ ")"
    Shr a n    -> "(" ++ show a ++ " >> " ++ show n ++ ")"

data Ty = W32 | I32 | W64 | I64

class Typed t where
    typeOf :: Expr t -> Ty
    fromMStack :: [Word32] -> Maybe t
    fromWStack :: [W.Value] -> Maybe t
    wasmPush   :: t -> W.Instruction i
    bitsize    :: t -> W.BitSize
    isSignedTy :: t -> Bool

instance Typed Word32 where
    typeOf _ = W32
    fromMStack = listToMaybe
    fromWStack s = case s of
      W.VI32 w : _ -> Just w
      _            -> Nothing
    wasmPush = W.I32Const
    bitsize _ = W.BS32
    isSignedTy _ = False
instance Typed Int32 where
    typeOf _ = I32
    fromMStack = fmap fromIntegral . listToMaybe
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word32)
    wasmPush i = W.I32Const (fromIntegral i)
    bitsize _ = W.BS32
    isSignedTy _ = True
instance Typed Word64 where
    typeOf _ = W64
    fromMStack (hi:lo:_) = Just $ fromFakeW64 (FakeW64 hi lo)
    fromMStack _         = Nothing
    fromWStack s = case s of
      W.VI64 w : _ -> Just w
      _            -> Nothing
    wasmPush = W.I64Const
    bitsize _ = W.BS64
    isSignedTy _ = False
instance Typed Int64 where
    typeOf _ = I64
    fromMStack = fmap fromIntegral . (fromMStack :: [Word32] -> Maybe Word64)
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word64)
    wasmPush w = W.I64Const (fromIntegral w)
    bitsize _ = W.BS64
    isSignedTy _ = True

exprToWasm :: Typed t => Expr t -> W.Module
exprToWasm expr = W.Module
  { types = [W.FuncType [] [retty]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] (exprWasm expr) ]
  , tables = [], mems = [], globals = [], elems = [], datas = [], imports = []
  }

  where retty = case typeOf expr of
          W32 -> W.I32
          I32 -> W.I32
          W64 -> W.I64
          I64 -> W.I64

        exprWasm :: Typed t => Expr t -> [W.Instruction Natural]
        exprWasm e = case e of
            ConstU32 w -> [W.I32Const w]
            ConstI32 w -> [W.I32Const (fromIntegral w)]
            ConstU64 w -> [W.I64Const w]
            ConstI64 w -> [W.I64Const (fromIntegral w)]
            Add a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IAdd
                         I32 -> W.IBinOp W.BS32 W.IAdd
                         W64 -> W.IBinOp W.BS64 W.IAdd
                         I64 -> W.IBinOp W.BS64 W.IAdd
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Sub a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.ISub
                         I32 -> W.IBinOp W.BS32 W.ISub
                         W64 -> W.IBinOp W.BS64 W.ISub
                         I64 -> W.IBinOp W.BS64 W.ISub
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Mul a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IMul
                         I32 -> W.IBinOp W.BS32 W.IMul
                         W64 -> W.IBinOp W.BS64 W.IMul
                         I64 -> W.IBinOp W.BS64 W.IMul
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Div a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IDivU
                         I32 -> W.IBinOp W.BS32 W.IDivS
                         W64 -> W.IBinOp W.BS64 W.IDivU
                         I64 -> W.IBinOp W.BS64 W.IDivS
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Shr a n    ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShrU)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShrS)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShrU)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShrS)
                  xs = [k, op]
              in exprWasm a ++ xs
            Shl a n    ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShl)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShl)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShl)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShl)
                  xs = [k, op]
              in exprWasm a ++ xs

randomExpr :: forall a. (Arbitrary a, Num a, Eq a) => Int -> (a -> Expr a) -> Maybe Bool -> Gen (Expr a)
randomExpr 0 konst _ = konst <$> arbitrary
randomExpr k konst mwithShrDiv = frequency $
  [ (1, randomExpr 0 konst mwithShrDiv)
  , (7, Add <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (7, Sub <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (5, Mul <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (3, Shl <$> randomExpr (k-1) konst mwithShrDiv <*> chooseInt (1, 31))
  ] ++
  case mwithShrDiv of
    Just True ->
      [ (4, Div <$> randomExpr (k-1) konst mwithShrDiv <*> (konst . getNonZero <$> arbitrary))
      , (2, Shr <$> randomExpr (k-1) konst mwithShrDiv <*> chooseInt (1, 31))
      ]
    Just False ->
      [ (4, Div <$> randomExpr 0 konst mwithShrDiv <*> (konst . getNonZero <$> arbitrary))
      , (2, Shr <$> randomExpr 0 konst mwithShrDiv <*> chooseInt (1, 3))
      ] -- hopefully those more modest generators are enough to avoid the annoying corner cases...
    Nothing -> [ (2, Shr <$> randomExpr 0 konst mwithShrDiv <*> chooseInt (1, 3)) ]

exprDepth :: Int
exprDepth = 4

instance Arbitrary (Expr Word32) where
  arbitrary = randomExpr exprDepth ConstU32 (Just True)
instance Arbitrary (Expr Int32) where
  arbitrary = randomExpr exprDepth ConstI32 (Just True)
instance Arbitrary (Expr Word64) where
  arbitrary = randomExpr exprDepth ConstU64 (Just True)
instance Arbitrary (Expr Int64) where
  arbitrary = randomExpr exprDepth ConstI64 (Just True)

exprEvalCompile :: forall t. (Integral t, Bits t, Typed t, Show t) => Expr t -> Property
exprEvalCompile e = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "expr: " ++ show e ++ " ===")
  let wmod = exprToWasm e
  Just reference <- fromWStack <$> Miden.simulateWASM wmod
  when debug $ putStrLn $ "result = " ++ show reference
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ T.putStrLn (pShow mmod)
    Miden.runMiden mmod
  when debug $ print mres
  case mres of
    Left err -> error ("exprEvalCompile got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> t -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"

data CmpOp = Lt | Gt | Lte | Gte | Eq | Neq
  deriving (Eq, Show, Enum, Bounded)

prettyCmpOp :: CmpOp -> String
prettyCmpOp o = case o of
  Lt  -> "<"
  Gt  -> ">"
  Lte -> "<="
  Gte -> ">="
  Eq  -> "=="
  Neq -> "/="

allCmpOps :: [CmpOp]
allCmpOps = [minBound .. maxBound]

instance Arbitrary CmpOp where
  arbitrary = elements allCmpOps

runCmpOp :: Ord a => CmpOp -> a -> a -> Bool
runCmpOp op = opFun
  where opFun = case op of
          Lt  -> (<)
          Gt  -> (>)
          Lte -> (<=)
          Gte -> (>=)
          Eq  -> (==)
          Neq -> (/=)

data Cmp a = Cmp a a CmpOp

instance Show a => Show (Cmp a) where
  show (Cmp a b op) = unwords [show a, prettyCmpOp op, show b]

instance Arbitrary a => Arbitrary (Cmp a) where
  arbitrary = Cmp <$> arbitrary <*> arbitrary <*> arbitrary

cmpToWasm :: Typed a => Cmp a -> W.Module
cmpToWasm (Cmp a b op) = W.Module
  { types = [W.FuncType [] [W.I32]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] instrs ]
  , tables = [], mems = [], globals = [], elems = [], datas = [], imports = []
  }
  where instrs = [ wasmPush a, wasmPush b, W.IRelOp sz relop ]
        sz = bitsize a
        signedTy = isSignedTy a
        relop = case op of
          Lt  -> if signedTy then W.ILtS else W.ILtU
          Gt  -> if signedTy then W.IGtS else W.IGtU
          Lte -> if signedTy then W.ILeS else W.ILeU
          Gte -> if signedTy then W.IGeS else W.IGeU
          Eq  -> W.IEq
          Neq -> W.INe

cmpTest :: forall a. (Arbitrary a, Eq a, Show a, Typed a) => Cmp a -> Property
cmpTest cmp@(Cmp x y cmpop) = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "expr: " ++ show x ++ " " ++ prettyCmpOp cmpop ++ " " ++ show y ++ " ===")
  let wmod = cmpToWasm cmp
  Just reference <- fromWStack @Word32 <$> Miden.simulateWASM wmod
  when debug $
    putStrLn $ "result = " ++ show reference ++ " (" ++
               show (reference == 1) ++ ")"
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ T.putStrLn (pShow mmod)
    Miden.runMiden mmod
  when debug $ print mres
  case mres of
    Left err -> error ("cmpTest got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> Word32 -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"

-- | Set the following to True for printing WASM/MASM modules, expressions, results, etc
debug :: Bool
debug = False
-- debug = True

-- To run more examples, use something like
-- cabal test --test-option=--qc-max-success=10000 arith-test
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hspec $ do
    describe "starkify translates arithmetic operations correctly" $ do
      prop "for unsigned 32 bits integers" $
        exprEvalCompile @Word32
      prop "for signed 32 bits integers" $
        exprEvalCompile @Int32
      prop "for unsigned 64 bits integers" $
        exprEvalCompile @Word64
      prop "for signed 64 bits integer" $
        exprEvalCompile @Int64
    describe "starkify translates comparison operators correctly" $ do
      prop "for unsigned 32 bits integers" $
        cmpTest @Word32
      prop "for signed 32 bits integers" $
        cmpTest @Int32
      prop "for unsigned 64 bits integers" $
        cmpTest @Word64
      prop "for signed 64 bits integer" $
        cmpTest @Int64
