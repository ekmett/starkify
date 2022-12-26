{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Data.Word

import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate as WASM
import qualified MASM
import qualified MASM.Interpreter as MASM
import qualified MASM.Miden as Miden
import qualified WASM.WasmTime as WasmTime

simulateWASM :: WASM.Module -> IO WasmTime.WasmResult
simulateWASM m = case WASM.validate m of
    Left err -> error ("WASM typechecker: " ++ show err)
    Right _ -> do
      r <- WasmTime.runModule m Nothing
      case r of
          Left err -> error ("WASM interpreter: " ++ err)
          Right a ->  return a

simulateMASM :: MASM.Module -> Either String ([Word32], MASM.Mem)
simulateMASM = MASM.runInterp . MASM.interpret

runMiden :: MASM.Module -> IO (Either String [Word32])
runMiden = Miden.runMiden
