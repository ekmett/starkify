{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Language.Wasm.Interpreter qualified as WASM
import Language.Wasm.Structure qualified as WASM
import Language.Wasm.Validate qualified as WASM

import MASM qualified
import MASM.Interpreter qualified as MASM
import MASM.Miden qualified as Miden
import Data.Word

simulateWASM :: WASM.Module -> IO (Maybe [WASM.Value])
simulateWASM m = case WASM.validate m of
    Left err -> error ("WASM typechecker: " ++ show err)
    Right mvalid -> do
      let store = WASM.emptyStore
          imports = WASM.emptyImports
      (r, store') <- WASM.instantiate store imports mvalid
      case r of
          Left err -> error ("WASM interpreter: " ++ err)
          Right minstance -> WASM.invokeExport store' minstance "main" []

simulateMASM :: MASM.Module -> Either String ([Word32], MASM.Mem)
simulateMASM = MASM.runInterp . MASM.interpret

runMiden :: MASM.Module -> IO (Either String [Word32])
runMiden = Miden.runMiden
