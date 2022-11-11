{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

module W2M where

import Data.Bits
import Data.ByteString.Lazy qualified as BS
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (Text, replace)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Word (Word8, Word32)
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W

import MASM qualified as M
import MASM.Interpreter (toFakeW64, FakeW64 (..))

import Validation


type WasmAddr = Int
type MasmAddr = Word32

data Ty = TyInt32 | TyInt64
  deriving (Eq, Show)

funName :: Id -> String
funName i = "fun" ++ show i

toMASM :: W.Module -> V M.Module
toMASM m = do
  -- TODO: don't throw away main's type, we might want to check it? and locals? they're probably going to be the inputs...?
  globalsInit <- getGlobalsInit
  datasInit <- getDatasInit

  M.Module ["std::sys", "std::math::u64"]
    <$> fmap catMaybes (traverse fun2MASM sortedFuns)
    <*> return (M.Program (globalsInit ++ datasInit ++ [ M.Exec "main" ] ++ stackCleanUp))

  where globalsAddrMap :: Vector MasmAddr
        memBeginning :: MasmAddr
        (globalsAddrMap, memBeginning) = (\(xs, n) -> (V.fromList xs, n)) $ foldl' f ([], 0) (W.globals m)
          where f (xs, n) globl_i =
                  let ncells = case W.globalType globl_i of
                                 W.Const t -> numCells t
                                 W.Mut   t -> numCells t
                  in (xs ++ [n], n+ncells)

        callGraph :: Map Text (Set Text)
        callGraph = Map.unionsWith (<>)
          [ Map.singleton caller (Set.singleton callee)
          | (caller, W.Function _ _ instrs) <- Map.toList functionsMap
          , W.Call k <- instrs
          , callee <- maybeToList $ Map.lookup (fromIntegral k) functionNamesMap
          ]

        enumerate x = x : concatMap enumerate [ y | y <- maybe [] Set.toList (Map.lookup x callGraph) ]
        sortedFuns = catMaybes . map (\fn -> (fn,) <$> Map.lookup fn functionsMap) $ 
          let xs = enumerate "main"
              rxs = reverse xs
              go [] _ = []
              go (a:as) !visited
                | a `Set.member` visited = go as visited
                | otherwise = a : go as (Set.insert a visited)
          in go rxs Set.empty

        numCells :: W.ValueType -> Word32
        numCells t = case t of
          W.I32 -> 1
          W.I64 -> 2
          _ -> error "numCells called on non integer value type"

        getDatasInit :: V [M.Instruction]
        getDatasInit = concat <$> traverse getDataInit (W.datas m)
        
        getDataInit :: W.DataSegment -> V [M.Instruction]
        getDataInit (W.DataSegment 0 offset_wexpr bytes) = do
          offset_mexpr <- translateInstrs mempty offset_wexpr
          pure $ offset_mexpr ++
                 [ M.Push 4, M.IDiv             -- [offset_bytes/4, ...]
                 , M.Push memBeginning, M.IAdd  -- [offset_bytes/4+memBeginning, ...] =
                 ] ++                           -- [addr_u32, ...]
                 writeW32s (BS.unpack bytes) ++ -- [addr_u32+len(bytes)/4, ...]
                 [ M.Drop ]                     -- [...]
        getDataInit _ = badNoMultipleMem

        writeW32s :: [Word8] -> [M.Instruction]
        writeW32s [] = []
        writeW32s (a:b:c:d:xs) =
          let w = foldl' (.|.) 0 [ shiftL (fromIntegral x) (8 * i)
                                 | (i, x) <- zip [0..] [a,b,c,d]
                                 ]
          in [ M.Dup 0 -- [addr_u32, addr_u32, ...]
             , M.Push w -- [w, addr_u32, addr_u32, ...]
             , M.Swap 1 -- [addr_u32, w, addr_u32, ...]
             , M.MemStore Nothing -- [w, addr_u32, ...]
             , M.Drop -- [addr_u32, ...]
             , M.Push 1, M.IAdd -- [addr_u32+1, ...]
             ] ++ writeW32s xs
        writeW32s xs = writeW32s $ xs ++ replicate (4-length xs) 0

        getGlobalsInit :: V [M.Instruction]
        getGlobalsInit = concat <$> traverse getGlobalInit (zip [0..] (W.globals m))

        getGlobalInit :: (Int, W.Global) -> V [M.Instruction]
        getGlobalInit (k, g) = do
          xs <- translateInstrs mempty (W.initializer g)
          storeInstrs <- translateInstr mempty (W.SetGlobal $ fromIntegral k)
          return (xs ++ storeInstrs)

        getGlobalTy k = case t of
            W.I32 -> TyInt32
            W.I64 -> TyInt64
            _     -> error "unsupported global type"

          where t = case W.globalType (W.globals m !! fromIntegral k) of
                      W.Const ct -> ct
                      W.Mut mt -> mt

        functionsMap :: Map Text W.Function
        functionsMap = Map.fromList
          [ (fixName fname, W.functions m !! fromIntegral i)
          | W.Export fname (W.ExportFunc i) <- W.exports m
          ]

        emptyFunctions :: Set Text
        emptyFunctions = Set.fromList
          [ fname
          | (fname, W.Function _ _ []) <- Map.toList functionsMap
          ]

        functionNamesMap :: Map Int Text
        functionNamesMap = Map.fromList
          [ (fromIntegral i, fixName fname)
          | W.Export fname (W.ExportFunc i) <- W.exports m
          ]

        functionTypesMap :: Map Text W.FuncType
        functionTypesMap =
          fmap (\(W.Function tyIdx _ _) -> W.types m !! fromIntegral tyIdx) functionsMap

        fixName = replace "__" "zz__"

        fun2MASM :: (Text, W.Function) -> V (Maybe M.Proc)
        fun2MASM (_fname, W.Function _         _         []) = return Nothing
        fun2MASM (fname, W.Function _funTyIdx localsTys body) = do
          let wasm_args = maybe [] W.params (Map.lookup fname functionTypesMap)
              wasm_locals = localsTys

              localAddrMap :: Map Natural [Word32]
              (localAddrMap, nlocalCells) =
                foldl' (\(addrs, cnt) (k, ty) -> case ty of
                           W.I32 -> (Map.insert k [cnt] addrs, cnt+1)
                           W.I64 -> (Map.insert k [cnt, cnt+1] addrs, cnt+2)
                           _     -> error $ "localAddrMap: floating point local var?"
                       )
                       (Map.empty, 0)
                       (zip [0..] (wasm_args ++ wasm_locals))
              -- the function starts by populating the first nargs local vars
              -- with the topmost nargs values on the stack, removing them from
              -- the stack as it goes. it assumes the value for the first arg
              -- was pushed first, etc, with the value for the last argument
              -- being pushed last and therefore popped first.

              prelude = reverse $ concat 
                [ case Map.lookup (fromIntegral k) localAddrMap of
                    Just is -> concat [ [ M.Drop, M.LocStore i ] | i <- is ]
                    _ -> error ("impossible: prelude of procedure " ++ show fname ++ ", local variable " ++ show k ++ " not found?!")
                | k <- [0..(length wasm_args - 1)]
                ]
          instrs <- translateInstrs localAddrMap body
          return $ Just (M.Proc fname (fromIntegral nlocalCells) (prelude ++ instrs))


        translateInstrs :: Map Natural [Word32] -> W.Expression -> V [M.Instruction]
        translateInstrs locals = concatMapA (translateInstr locals)

        translateInstr :: Map Natural [Word32] -> W.Instruction Natural -> V [M.Instruction]
        translateInstr _ (W.Call i) = case Map.lookup (fromIntegral i) functionNamesMap of
          Nothing -> badWasmFunctionCallIdx (fromIntegral i)
          Just fname | fname `Set.notMember` emptyFunctions ->
            (pure.pure) (M.Exec fname)
          Just _ -> pure []
        translateInstr _ (W.I32Const w32) = (pure.pure) (M.Push w32)
        translateInstr _ (W.IBinOp bitsz op) = fmap pure (translateIBinOp bitsz op)
        translateInstr _ W.I32Eqz = (pure.pure) (M.IEq (Just 0))
        translateInstr _ (W.IRelOp bitsz op) = fmap pure (translateIRelOp bitsz op)
        translateInstr _ W.Select = (pure.pure) $
          M.IfTrue [M.Drop] [M.Swap 1, M.Drop]
        translateInstr _ (W.I32Load (W.MemArg offset align)) = case align of
          2 -> pure [ M.Push 4
                    , M.IDiv
                    , M.Push (fromIntegral offset `div` 4)
                    , M.IAdd
                    , M.Push memBeginning
                    , M.IAdd
                    , M.MemLoad Nothing
                    ]
          _ -> unsupportedMemAlign align
        translateInstr _ (W.I32Store (W.MemArg offset align)) = case align of
          -- we need to turn [val, byte_addr, ...] of wasm into [u32_addr, val, ...]
          2 -> pure [ M.Swap 1
                    , M.Push 4
                    , M.IDiv
                    , M.Push (fromIntegral offset `div` 4)
                    , M.IAdd
                    , M.Push memBeginning
                    , M.IAdd
                    , M.MemStore Nothing
                    , M.Drop
                    ]
          _ -> unsupportedMemAlign align
        translateInstr localAddrs (W.GetLocal k) = case Map.lookup k localAddrs of
          Just is -> pure (map M.LocLoad is)
          _ -> error ("impossible: local variable " ++ show k ++ " not found?!")

        translateInstr localAddrs (W.SetLocal k) = case Map.lookup k localAddrs of
          Just is -> pure $ concat 
            [ [ M.LocStore i
              , M.Drop
              ]
            | i <- reverse is
            ]
          _ -> error ("impossible: local variable " ++ show k ++ " not found?!")
        translateInstr localAddrs (W.TeeLocal k) =
          translateInstrs localAddrs [W.SetLocal k, W.GetLocal k]

        translateInstr _ (W.GetGlobal k) = case getGlobalTy k of
          TyInt32 -> (pure.pure) (M.MemLoad . Just $ globalsAddrMap V.! fromIntegral k)
          TyInt64 -> pure [ M.MemLoad . Just $ (globalsAddrMap V.! fromIntegral k)
                          , M.MemLoad . Just $ (globalsAddrMap V.! fromIntegral k) + 1
                          ]
        translateInstr _ (W.SetGlobal k) = case getGlobalTy k of
          TyInt32 -> pure [ M.MemStore . Just $ globalsAddrMap V.! fromIntegral k 
                          , M.Drop
                          ]
          TyInt64 -> pure [ M.MemStore . Just $ (globalsAddrMap V.! fromIntegral k) + 1
                          , M.Drop
                          , M.MemStore . Just $ (globalsAddrMap V.! fromIntegral k)
                          , M.Drop
                          ]

        -- https://maticnetwork.github.io/miden/user_docs/stdlib/math/u64.html
        -- 64 bits integers are emulated by separating the high and low 32 bits.
        translateInstr _ (W.I64Const k) = pure
          [ M.Push k_lo, M.Push k_hi ]
          where FakeW64 k_hi k_lo = toFakeW64 k
        translateInstr _ (W.I64Load (W.MemArg offset align)) = case align of
          -- we need to turn [byte_addr, ...] of wasm into
          -- [u32_addr, ...] for masm, and then call mem_load
          -- twice (once at u32_addr, once at u32_addr+1)
          -- to get hi and lo 32 bits of i64 value.
          --
          -- u32_addr = (byte_addr / 4) + (offset / 4) + memBeginning
          3 -> pure [ M.Push 4, M.IDiv
                    , M.Push (fromIntegral offset `div` 4)
                    , M.IAdd
                    , M.Push memBeginning, M.IAdd -- [addr, ...]
                    , M.Dup 0 -- [addr, addr, ...]
                    , M.MemLoad Nothing -- [lo, addr, ...]
                    , M.Swap 1 -- [addr, lo, ...]
                    , M.Push 1, M.IAdd -- [addr+1, lo, ...]
                    , M.MemLoad Nothing -- [hi, lo, ...]
                    ]
          _ -> unsupportedMemAlign align
        translateInstr _ (W.I64Store (W.MemArg offset align)) = case align of
          -- we need to turn [val_hi, val_low, byte_addr, ...] of wasm into
          -- [u32_addr, val64_hi, val64_low, ...] for masm,
          -- and the call mem_store twice
          -- (once at u32_addr, once at u32_addr+1)
          -- to get hi and lo 32 bits of i64 value.
          3 -> pure [ M.Swap 1, M.Swap 2 -- [byte_addr, hi, lo, ...]
                    , M.Push 4, M.IDiv
                    , M.Push (fromIntegral offset `div` 4)
                    , M.IAdd
                    , M.Push memBeginning
                    , M.IAdd -- [addr, hi, lo, ...]
                    , M.Dup 0 -- [addr, addr, hi, lo, ...]
                    , M.Swap 2, M.Swap 1 -- [addr, hi, addr, lo, ...]
                    , M.Push 1, M.IAdd -- [addr+1, hi, addr, lo, ...]
                    , M.MemStore Nothing -- [hi, addr, lo, ...]
                    , M.Drop -- [addr, lo, ...]
                    , M.MemStore Nothing -- [lo, ...]
                    , M.Drop -- [...]
                    ]
          _ -> unsupportedMemAlign align

        -- turning an i32 into an i64 in wasm corresponds to pushing 0 on the stack.
        -- let's call the i32 'i'. before executing this, the stack looks like [i, ...],
        -- and after like: [0, i, ...].
        -- Since an i64 'x' on the stack is in Miden represented as [x_hi, x_lo], pushing 0
        -- effectively grabs the i32 for the low bits and sets the high 32 bits to 0.
        translateInstr _ W.I64ExtendSI32 = (pure.pure) (M.Push 0)
        translateInstr _ W.I64Eqz = (pure.pure) M.IEqz64

        translateInstr _ i = unsupportedInstruction i

        translateIBinOp :: W.BitSize -> W.IBinOp -> V M.Instruction
        -- TODO: the u64 module actually provides implementations of many binops for 64 bits
        -- values.
        translateIBinOp W.BS64 op = case op of
          W.IAdd -> return M.IAdd64
          W.ISub -> return M.ISub64
          W.IMul -> return M.IMul64
          _      -> unsupported64Bits op
        translateIBinOp W.BS32 op = case op of
          W.IAdd  -> return M.IAdd
          W.ISub  -> return M.ISub 
          W.IMul  -> return M.IMul
          W.IShl  -> return M.IShL
          W.IShrU -> return M.IShR
          W.IAnd  -> return M.IAnd
          W.IOr   -> return M.IOr
          W.IXor  -> return M.IXor
          _       -> unsupportedInstruction (W.IBinOp W.BS32 op)

        translateIRelOp :: W.BitSize -> W.IRelOp -> V M.Instruction
        translateIRelOp W.BS64 op = case op of
          W.IEq  -> return M.IEq64
          W.INe  -> return M.INeq64
          W.ILtU -> return M.ILt64
          W.IGtU -> return M.IGt64
          W.ILeU -> return M.ILte64
          W.IGeU -> return M.IGte64
          _      -> unsupported64Bits op
        translateIRelOp W.BS32 op = case op of
          W.IEq  -> return (M.IEq Nothing)
          W.INe  -> return M.INeq
          W.ILtU -> return M.ILt
          W.IGtU -> return M.IGt
          W.ILeU -> return M.ILte
          W.IGeU -> return M.IGte
          _      -> unsupportedInstruction (W.IRelOp W.BS32 op)

        -- necessary because of https://github.com/maticnetwork/miden/issues/371
        -- the stack must be left with exactly 16 entries at the end of the program
        -- for proof generaton, so we remove a bunch of entries accordingly.
        stackCleanUp :: [M.Instruction]
        stackCleanUp = [M.TruncateStack]
        -- stackCleanUp n = concat $ replicate n [ M.Swap n', M.Drop ]
        --   where n' = fromIntegral n

concatMapA :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatMapA f = fmap concat . traverse f
