{-# LANGUAGE TypeApplications #-}
-- | Run WASM code with wasmtime
module WASM.WasmTime where

import System.Exit
import System.FilePath
import System.Process
import System.IO.Temp (withSystemTempDirectory)

import Text.Read
import Data.Maybe
import Data.Word

import Language.Wasm.Interpreter (Value (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Language.Wasm.Binary as WASM
import qualified Language.Wasm.Structure as WASM

type FunName = String
type WasmResult = [Value]

runModule :: WASM.Module -> Maybe FunName -> IO (Either String WasmResult)
runModule wmod mfun = withSystemTempDirectory "starkify-wasmtime-XXX" $ \dir -> do
    let path = dir </> "mod.wasm"
    LBS.writeFile path (WASM.dumpModuleLazy wmod)
    either (pure . Left) (run path fun) getRetTys

    where fun = fromMaybe "main" mfun
          getRetTys = do
            fidx <- maybe (Left $ "couldn't find function '" ++ fun ++ "' in WASM module") Right findFun
            let WASM.Function ftyidx _ _ = WASM.functions wmod !! fromIntegral fidx
                WASM.FuncType _ results = WASM.types wmod !! fromIntegral ftyidx
            return results
          findFun = listToMaybe
              [ i
              | WASM.Export name (WASM.ExportFunc i) <- WASM.exports wmod
              , name == T.pack fun
              ]

run :: FilePath -> FunName -> [WASM.ValueType] -> IO (Either String WasmResult)
run fp fun retTys = do
    (ex, wout, werr) <- readProcessWithExitCode "wasmtime" ["run", fp, "--invoke", fun] ""
    case ex of
        ExitSuccess -> return (traverse readValue $ zip retTys $ filter (/= "") $ lines wout)
        ExitFailure e -> return (Left $ "wasmtime run failed: " ++ show (e, wout, werr))

    where readValue (ty, str) = case ty of
              WASM.I32 -> maybe (Left $ "couldn't read Word32 from: " ++ str) (Right . VI32) (readMaybe @Word32 str)
              WASM.I64 -> maybe (Left $ "couldn't read Word64 from: " ++ str) (Right . VI64) (readMaybe @Word64 str)
              _        -> Left "floating points not supported yet"
