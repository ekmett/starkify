module TestFiles where

import Eval (simulateWASM)
import MASM.Interpreter (runInterp, interpret, FakeW64(..), fromFakeW64)
import MASM.Miden
import Validation (runValidation)
import W2M (toMASM)

import Control.Exception
import Control.Monad
import Data.List (isSuffixOf, sort)
import Data.Word
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.IO.Temp
import Test.Hspec
import Text.Read

import Data.ByteString.Lazy qualified as LBS
import Language.Wasm qualified as Wasm
import Language.Wasm.Interpreter qualified as Wasm

withCompiledC
 :: FilePath -- path to .c file
 -> (FilePath -> IO a) -- what to do with .wasm file
 -> IO a
withCompiledC dotc f = do
    wasmPath <- writeSystemTempFile (takeFileName dotc <.> "wasm") ""
    (do (ex, clangout, clangerr) <- readProcessWithExitCode "clang-14" (clangOpts ++ ["-o", wasmPath, dotc]) ""
        case ex of
            ExitSuccess -> f wasmPath
            ExitFailure e -> error $ "clang failed: " ++ show (e, clangout, clangerr)
      ) `finally` removeFile wasmPath

    where clangOpts = [ "--target=wasm32"
                      , "--no-standard-libraries"
                      , "-O1"
                      , "-Wl,--no-entry", "-Wl,--export-all", "-Wl,--strip-all"
                      ]

withWasmFromWat
  :: FilePath -- path to .wat file
  -> (Wasm.Module -> IO a)
  -> IO a
withWasmFromWat watFile f = do
  watCode <- LBS.readFile watFile
  case Wasm.parse watCode of
    Left err -> error ("couldn't parse textual WASM from " ++ watFile ++ ": " ++ show err)
    Right wasmMod -> f wasmMod

withWasmFromC
  :: FilePath -- path to .c file
  -> (Wasm.Module -> IO a)
  -> IO a
withWasmFromC dotc f = withCompiledC dotc $ \wasmFile -> do
    wasmCode <- LBS.readFile wasmFile
    case Wasm.decodeLazy wasmCode of
        Left err -> error ("couldn't parse binary WASM from " ++ wasmFile ++ ": " ++ err)
        Right wasmMod -> f wasmMod

withWasm
  :: FilePath -- .wat or .c file
  -> (Wasm.Module -> IO a)
  -> IO a
withWasm fp f
  | takeExtension fp == ".c"   = withWasmFromC fp f
  | takeExtension fp == ".wat" = withWasmFromWat fp f
  | otherwise = error ("withWasm cannot handle testfile: " ++ fp)

compareWasmMasmResult :: FilePath -> Wasm.Module -> Expectation
compareWasmMasmResult expectedOutFile wmod = do
  mwasmres <- simulateWASM wmod
  mexpectedOut <- readMaybe <$> readFile expectedOutFile
  expectedOut <- case mexpectedOut of
    Nothing -> error $ "couldn't parse " ++ expectedOutFile ++ " as [Word32]"
    Just res -> return res
  mmod <- runValidation (toMASM wmod)
  mmasmres <- runMiden mmod
  case (mwasmres, mmasmres) of
    (Just vals, Right stack) -> checkOutput expectedOut stack >> compareStacks (reverse vals) stack 0
    _ -> error ("unexpected results: " ++ show (mwasmres, mmasmres))

  where compareStacks [] masmstack _k = filter (/=0) masmstack `shouldBe` []
        compareStacks (Wasm.VI32 wasm_w32 : wasm_vs) (masm_w32 : masm_vs) k = do
          wasm_w32 `shouldBe` masm_w32
          compareStacks wasm_vs masm_vs (k+1)
        compareStacks (Wasm.VI64 wasm_w64 : wasm_vs) (masm_w64_hi:masm_w64_lo:masm_vs) k = do
          let masm_w64 = fromFakeW64 (FakeW64 masm_w64_hi masm_w64_lo)
          wasm_w64 `shouldBe` masm_w64
          compareStacks wasm_vs masm_vs (k+1)
        compareStacks wasmstack masmstack _k = error $ "cannot compare stacks: " ++ show (wasmstack, masmstack)

checkOutput :: [Word32] -> [Word32] -> Expectation
checkOutput expected actual = do
          take (length expected) actual `shouldBe` expected
          drop (length expected) actual `shouldBe` replicate (16 - length expected) 0

genProofAndVerify :: Wasm.Module -> Expectation
genProofAndVerify wmod = do
  mmod <- runValidation (toMASM wmod)
  (midenOut, midenProof, midenProgHash) <- runMidenProve mmod
  verifRes <- runMidenVerify midenOut midenProof midenProgHash
  verifRes `shouldBe` Nothing
  -- TODO cleanup temp files

checkInterpreter :: FilePath -> Wasm.Module -> Expectation
checkInterpreter expectedOutFile wmod = do
  mexpectedOut <- readMaybe <$> readFile expectedOutFile
  expectedOut <- case mexpectedOut of
    Nothing -> error $ "couldn't parse " ++ expectedOutFile ++ " as [Word32]"
    Just res -> return res
  mmod <- runValidation (toMASM wmod)
  case runInterp (interpret mmod) of
    Right (stack, _mem) -> checkOutput expectedOut stack
    Left err -> error ("checkInterpreter: " ++ err)

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    exists <- doesDirectoryExist (cwd </> "testfiles")
    unless exists $
      error "you must execute this testsuite from the root of the starkify directory"
    testfiles <- filter (not . (".out" `isSuffixOf`)) . sort <$> listDirectory (cwd </> "testfiles")
    hspec $
      sequence_ [ describe ("testfiles" </> fp) $ do
                    it "gives the same, correct result with wasm and miden" $
                      withWasm ("testfiles" </> fp) (compareWasmMasmResult ("testfiles" </> fp <.> "out"))
                    it "can be executed through Miden to get a proof which can be successfully verified" $
                      withWasm ("testfiles" </> fp) genProofAndVerify
                    -- it "can be executed by our interpreter and return the correct result" $
                    --   withWasm ("testfiles" </> fp) (checkInterpreter ("testfiles" </> fp <.> "out"))
                | fp <- testfiles
                ]
