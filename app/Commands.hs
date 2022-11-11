{-# LANGUAGE RecordWildCards #-}

module Commands where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (isPrefixOf)
import Data.Text.Lazy qualified as T
import Data.Word
import GHC.Generics
import Language.Wasm qualified as Wasm
import MASM (ppMASM)
import MASM.Miden (runMidenVerify)
import Options
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Text.Pretty.Simple (pShow)
import Validation
import W2M

runCommand :: Command -> IO ()
runCommand (Build buildOpts) = runBuild buildOpts
runCommand (Run runOpts) = runRun runOpts
runCommand (Verify verifyOpts) = runVerify verifyOpts

---------------

runBuild :: BuildOpts -> IO ()
runBuild BuildOpts {..} = (>>) (dump ("Compiling " ++ buildInFile ++ " ...")) $ withWasm buildInFile $ \wasmFile wasmMod -> do
  when dumpWasm $ case wasmFile of
    DotWat fp -> dumpFile "WASM code" fp
    DotWasm fp -> withWatFromWasm fp $ \watFP ->
      dumpFile "WASM code" watFP

  when dumpWasmAst $
    dumps "WASM AST" (lines . T.unpack $ pShow wasmMod)

  masmMod <- runValidation $ do
    standardValidator wasmMod
    toMASM checkImports wasmMod

  when dumpMasmAst $
    dumps "MASM AST" (lines . T.unpack $ pShow masmMod)

  let masmCode = ppMASM masmMod

  writeFile buildOutMasmFile masmCode

  when dumpMasm $
    dumpFile "MASM code" buildOutMasmFile

  dump ("Compilation done: " ++ buildOutMasmFile)

  when brunToo $
    runRun $
      RunOpts
        { runMasmFile = buildOutMasmFile,
          runProofFile = buildOutMasmFile <.> "proof",
          runOutFile = buildOutMasmFile <.> "out",
          rverifyToo = bverifyToo
        }
  where
    standardValidator wasm_mod =
      case Wasm.validate wasm_mod of
        Left err -> failsStandardValidation err
        Right _validMod -> return ()

runRun :: RunOpts -> IO ()
runRun RunOpts {..} = do
  dump ("Execution of program " ++ runMasmFile ++ " ...")
  (ex, midenout, midenerr) <-
    readProcessWithExitCode
      "miden"
      ["prove", "--assembly", runMasmFile, "-o", runOutFile, "-p", runProofFile]
      ""
  case ex of
    ExitFailure n -> do
      dump ("miden prove failed with exit code: " ++ show n)
      dumps "miden prove stdout" (lines midenout)
      dumps "miden prove stderr" (lines midenerr)
      error "miden prove failed"
    ExitSuccess ->
      case map (takeWhile (/= '.') . drop (length hashPrefix)) $ filter (hashPrefix `isPrefixOf`) (lines midenout) of
        [hash] -> do
          ok (runOutFile, runProofFile, hash)
          when rverifyToo $
            runVerify $
              VerifyOpts
                { verifyOutFile = runOutFile,
                  verifyProofFile = runProofFile,
                  verifyHash = hash
                }
        _ -> do
          dumps "miden prove output" (lines midenout)
          error "couldn't determine hash from miden prove output"
  where
    hashPrefix = "Proving program with hash "

    ok (out, proof, hash) = do
      stack <- getStack out
      putStrLn $
        unlines
          [ "Successfully generated proof " ++ proof,
            "Output of the program stored in " ++ out,
            "Program hash: " ++ hash,
            "Final state of the stack:",
            "\t" ++ show stack
          ]

runVerify :: VerifyOpts -> IO ()
runVerify VerifyOpts {..} = do
  dump ("Verification of proof " ++ verifyProofFile ++ " ...")
  r <- runMidenVerify verifyOutFile verifyProofFile verifyHash
  case r of
    Nothing -> dump "Verification successful"
    Just (ex, midenout, midenerr) -> do
      dump ("Verification failed (exit code " ++ show ex ++ ")")
      dumps "miden verify stdout" (lines midenout)
      dumps "miden verify stderr" (lines midenerr)
      error "miden verify failed"

data MidenOut = MidenOut
  { stack :: [String],
    overflow_addrs :: [String]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MidenOut

getStack :: FilePath -> IO [Word32]
getStack fp = do
  bytes <- LBS.readFile fp
  case Aeson.decode bytes of
    Nothing -> error ("couldn't decode " ++ fp ++ " as valid JSON")
    Just a -> return (map read (stack a))

---------------

withCompiledC ::
  FilePath -> -- path to .c file
  (FilePath -> IO a) -> -- what to do with .wasm file
  IO a
withCompiledC dotc operation = do
  withSystemTempDirectory "withCompiledC" $ \dir -> do
    let wasmPath = dir </> takeFileName dotc <.> "wasm"
    (ex, clangout, clangerr) <-
      readProcessWithExitCode
        "clang-14"
        (clangOpts ++ ["-o", wasmPath, dotc])
        ""
    case ex of
      ExitSuccess -> operation wasmPath
      ExitFailure e ->
        error $
          "clang failed: " ++ show (e, clangout, clangerr)
  where
    clangOpts =
      [ "--target=wasm32",
        "--no-standard-libraries",
        "-O1",
        "-Wl,--no-entry",
        "-Wl,--export-all",
        "-Wl,--strip-all"
      ]

withWatFromWasm ::
  FilePath -> -- path to .wasm file
  (FilePath -> IO a) -> -- what to do with .wat file
  IO a
withWatFromWasm wasmFile operation = do
  withSystemTempDirectory "withWatFromWasm" $ \dir -> do
    let watPath = dir </> takeFileName wasmFile <.> "wat"
    (ex, wasmout, wasmerr) <-
      readProcessWithExitCode
        "wasm2wat"
        [wasmFile, "-o", watPath]
        ""
    case ex of
      ExitSuccess -> operation watPath
      ExitFailure e ->
        error $ "wasm2wat failed: " ++ show (e, wasmout, wasmerr)

withWasmFromWat ::
  FilePath -> -- path to .wat file
  (WasmFile -> Wasm.Module -> IO a) ->
  IO a
withWasmFromWat watFile operation = do
  watCode <- LBS.readFile watFile
  case Wasm.parse watCode of
    Left err ->
      error
        ("couldn't parse textual WASM from " ++ watFile ++ ": " ++ show err)
    Right wasmMod -> operation (DotWat watFile) wasmMod

withWasmFromC ::
  FilePath -> -- path to .c file
  (WasmFile -> Wasm.Module -> IO a) ->
  IO a
withWasmFromC dotc operation = withCompiledC dotc $ \wasmFile -> do
  wasmCode <- LBS.readFile wasmFile
  case Wasm.decodeLazy wasmCode of
    Left err ->
      error
        ("couldn't parse binary WASM from " ++ wasmFile ++ ": " ++ err)
    Right wasmMod -> operation (DotWasm wasmFile) wasmMod

data WasmFile = DotWasm FilePath | DotWat FilePath
  deriving (Show)

withWasm ::
  FilePath -> -- .wat or .c file
  (WasmFile -> Wasm.Module -> IO a) ->
  IO a
withWasm fp operation = case takeExtension fp of
  ".c" -> withWasmFromC fp operation
  ".wat" -> withWasmFromWat fp operation
  ".wasm" -> do
    wasmCode <- LBS.readFile fp
    case Wasm.decodeLazy wasmCode of
      Left err -> error ("couldn't parse binary WASM from " ++ fp ++ ": " ++ err)
      Right wasmMod -> operation (DotWasm fp) wasmMod
  _ -> error ("cannot handle file: " ++ fp ++ ", only .c and .wat are supported")

---------------

dump :: String -> IO ()
dump = hPutStrLn stderr

dumps :: String -> [String] -> IO ()
dumps s xs = dump $ unlines (decorate s xs)

dumpFile :: String -> FilePath -> IO ()
dumpFile header fp = do
  s <- readFile fp
  dumps (header ++ " (" ++ fp ++ ")") (lines s)

decorate :: String -> [String] -> [String]
decorate str lns =
  [ dottedLine,
    wrapBars (alignCenter str'),
    dottedLine
  ]
    ++ map (wrapBars . padRight) lns
    ++ [dottedLine]
  where
    str' = "\ESC[0;33m" ++ str ++ "\ESC[0m"
    l = maximum $ map mylength (str' : lns)
    totalLen = l + 4
    dottedLine = replicate totalLen '-'
    wrapBars s = "| " ++ s ++ " |"
    alignCenter s =
      let leftPad = (l - mylength s) `div` 2
          rightPad = (l - mylength s) - leftPad
       in replicate leftPad ' ' ++ s ++ replicate rightPad ' '
    padRight s = s ++ replicate (l - mylength s) ' '

    mylength [] = 0
    mylength ('\ESC' : xs) = case dropWhile (/= 'm') xs of
      rest -> mylength (tail rest)
    mylength (_x : xs) = 1 + mylength xs
