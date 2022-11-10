{-# LANGUAGE OverloadedStrings #-}
module Eval where

import qualified Language.Wasm.Interpreter as WASM
import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate as WASM

import qualified MASM
import qualified MASM.Interpreter as MASM
import qualified MASM.Miden as Miden
import Data.Word

simulateWASM :: WASM.Module -> IO (Maybe [WASM.Value])
simulateWASM m = case WASM.validate m of
    Left err -> error (show err)
    Right mvalid -> do
      let store = WASM.emptyStore
          imports = WASM.emptyImports
      (r, store') <- WASM.instantiate store imports mvalid
      case r of
          Left err -> error err
          Right minstance -> WASM.invokeExport store' minstance "main" []

simulateMASM :: MASM.Module -> ([Word32], MASM.Mem)
simulateMASM = MASM.interpret

runMiden :: MASM.Module -> IO (Either String [Word32])
runMiden = Miden.runMiden
