{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module W2M where

import Control.Monad

import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text.Lazy (Text, replace)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Wasm.Structure as W
import qualified MASM as M
import Validation

funName :: Id -> String
funName i = "fun" ++ show i

toMASM :: W.Module -> V M.Module
toMASM m = do
  -- TODO: don't throw away main's type, we might want to check it? and locals? they're probably going to be the inputs...?
  (W.Function _mainty _mainlcls mainInstrs, funs) <- splitMain
  M.Module ["std::sys"] <$>
           fmap catMaybes (traverse fun2MASM (Map.toList funs))
           <*> (M.Program . (++ stackCleanUp 1) <$> translateInstrs mainInstrs)
    -- TODO: 1 because we assume 1 output for now, determine this from main's signature instead?

  where functionsMap :: Map Text W.Function
        functionsMap =  Map.fromList
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

        splitMain :: V (W.Function, Map Text W.Function)
        splitMain = case Map.splitLookup "main" functionsMap of
          (l, mmain, r) -> case mmain of
            Nothing      -> badNoMain
            Just mainFun -> return (mainFun, l <> r)

        fun2MASM :: (Text, W.Function) -> V (Maybe M.Proc)
        fun2MASM (_fname, W.Function _         _         []) = return Nothing
        fun2MASM (fname, W.Function _funTyIdx localsTys body) = do
          let nactuallocals = length localsTys
              nargs = Map.findWithDefault 0 fname
                . fmap (length . W.params)
                $ functionTypesMap
              nlocals = nactuallocals + nargs
              -- the function starts by populating the first nargs local vars
              -- with the topmost nargs values on the stack, removing them from
              -- the stack as it goes. it assumes the value for the first arg
              -- was pushed first, etc, with the value for the last argument
              -- being pushed last and therefore popped first.
              prelude = reverse $ concat
                [ [ M.Drop, M.LocStore (fromIntegral k) ]
                | k <- [0..nargs-1]
                ]
              -- TODO: figure out how the wasm lib parses 'call $foo (bar) (baz)'
              --       since it "linearizes" this into pushing bar, baz and calling foo,
              --       one instruction per step.
          instrs <- translateInstrs body
          return $ Just (M.Proc fname nlocals (prelude ++ instrs))

        translateInstrs :: W.Expression -> V [M.Instruction]
        translateInstrs = fmap catMaybes . traverse translateInstr

        translateInstr :: _ -> V (Maybe M.Instruction)
        translateInstr (W.Call i) = case Map.lookup (fromIntegral i) functionNamesMap of
          Nothing -> badWasmFunctionCallIdx (fromIntegral i)
          Just fname | fname `Set.notMember` emptyFunctions ->
            (pure.pure) (M.Exec fname)
          Just _ -> pure Nothing
        translateInstr (W.I32Const w32) = (pure.pure) (M.Push w32)
        translateInstr (W.GetLocal k) =  (pure.pure) (M.LocLoad (fromIntegral k))
        translateInstr (W.SetLocal k) =  (pure.pure) (M.LocStore (fromIntegral k))
        translateInstr (W.IBinOp bitsz op) = fmap pure (translateIBinOp bitsz op)
        translateInstr W.I32Eqz = (pure.pure) (M.EqConst 0)
        translateInstr (W.IRelOp bitsz op) = fmap pure (translateIRelOp bitsz op)
        translateInstr W.Select = (pure.pure) $
          M.IfTrue [M.Drop] [M.Swap 1, M.Drop]
        translateInstr (W.GetGlobal w32) = (pure.pure) (M.MemLoad $ fromIntegral w32)
        translateInstr (W.SetGlobal w32) = pure.pure $
          M.MemStore (fromIntegral w32)
        translateInstr (W.I32Store W.MemArg {offset}) = (pure.pure) $
          -- TODO(Matthias): this is not generally true. Fix.
          -- Implement wrap-around behaviour.
          -- TODO: also assert somehing about alignment and offset?
          M.MemStore (fromIntegral offset)

        translateInstr (W.I32Load W.MemArg {offset}) = (pure.pure) $
          -- TODO(Matthias): this is not generally true. Fix.
          -- Implement wrap-around behaviour.
          -- TODO: also assert somehing about alignment and offset?
          M.MemLoad (fromIntegral offset)

        translateInstr i = unsupportedInstruction i

        translateIBinOp :: W.BitSize -> W.IBinOp -> V M.Instruction
        translateIBinOp W.BS64 = unsupported64Bits
        translateIBinOp W.BS32 = \case
          W.IAdd  -> return M.IAdd
          W.ISub  -> return M.ISub
          W.IMul  -> return M.IMul
          W.IShl  -> return M.ShL
          W.IShrS -> return M.ShR
          W.IAnd  -> return M.And
          W.IOr   -> return M.Or
          W.IXor  -> return M.Xor
          op      -> unsupportedInstruction (W.IBinOp W.BS32 op)

        translateIRelOp :: W.BitSize -> W.IRelOp -> V M.Instruction
        translateIRelOp W.BS64 op = unsupported64Bits op
        translateIRelOp W.BS32 op = case op of
          W.IEq  -> return M.Eq
          W.INe  -> return M.Neq
          W.ILtS -> return M.Lt
          W.IGtS -> return M.Gt
          _      -> unsupportedInstruction (W.IRelOp W.BS32 op)

        -- necessary because of https://github.com/maticnetwork/miden/issues/371
        -- the stack must be left with exactly 16 entries at the end of the program
        -- for proof generaton, so we remove a bunch of entries accordingly.
        stackCleanUp :: Int -> [M.Instruction]
        stackCleanUp _ = [M.TruncateStack]
        -- stackCleanUp n = concat $ replicate n [ M.Swap n', M.Drop ]
        --   where n' = fromIntegral n
