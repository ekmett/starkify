module Options
  ( Command(..), BuildOpts(..), RunOpts(..), VerifyOpts(..)
  , getCommand
  ) where

import Options.Applicative

data Command
  = Build   BuildOpts
  | Run     RunOpts
  | Verify  VerifyOpts
  deriving Show

data BuildOpts = BuildOpts
  { buildInFile      :: FilePath
  , buildOutMasmFile :: FilePath
  , dumpWasm         :: Bool
  , dumpWasmAst      :: Bool
  , dumpMasm         :: Bool
  , dumpMasmAst      :: Bool
  , brunToo           :: Bool
  , bverifyToo        :: Bool
  } deriving Show

data RunOpts = RunOpts
  { runMasmFile  :: FilePath
  , runProofFile :: FilePath
  , runOutFile   :: FilePath
  , rverifyToo   :: Bool
  } deriving Show

data VerifyOpts = VerifyOpts
  { verifyProofFile :: FilePath
  , verifyOutFile   :: FilePath
  , verifyHash      :: String
  } deriving Show

getCommand :: IO Command
getCommand = execParser opts

opts :: ParserInfo Command
opts = info (parseCommand <**> helper) (progDesc "starkify - WASM to Miden VM compiler")

parseCommand :: Parser Command
parseCommand = hsubparser
  ( command "build"  (info (Build <$> buildOpts)
                           (progDesc "Compile C or WASM to MASM")
                     )
 <> command "run"    (info (Run <$> runOpts)
                           (progDesc "Run MASM programs and generate proofs")
                     )
 <> command "verify" (info (Verify <$> verifyOpts)
                           (progDesc "Verify a proof")
                     )
  )

buildOpts :: Parser BuildOpts
buildOpts = BuildOpts
        <$> strOption
            ( long "input"
           <> short 'i'
           <> metavar "FILE"
           <> help "path to .c or .wat (textual WASM) input file to compile"
            )
        <*> strOption
            ( long "output"
           <> short 'o'
           <> metavar "FILE"
           <> help "path to .masm output file"
            )
        <*> switch
            ( long "dump-wasm"
           <> short 'w'
           <> help "dump textual WASM code"
            )
        <*> switch
            ( long "dump-wasm-ast"
           <> help "dump the Haskell structures representing the WASM syntax tree"
            )
        <*> switch
            ( long "dump-masm"
           <> short 'm'
           <> help "dump textual WASM code"
            )
        <*> switch
            ( long "dump-masm-ast"
           <> help "dump textual WASM code"
            )
        <*> switch
            ( long "run"
           <> short 'r'
           <> help "run the program as well, generate a proof of execution along the way"
            )
        <*> switch
            ( long "verify"
           <> short 'v'
           <> help "verify the proof of execution for the program as well"
            )

runOpts :: Parser RunOpts
runOpts = RunOpts
      <$> strOption
            ( long "input"
           <> short 'i'
           <> metavar "FILE"
           <> help "path to .masm file to run"
            )
      <*> strOption
            ( long "proof"
           <> short 'p'
           <> metavar "FILE"
           <> help "path to a proof file to be produced, the proof is only generated when this option is set"
            )
      <*> strOption
            ( long "output"
           <> short 'o'
           <> metavar "FILE"
           <> help "path to file in which to store program output"
            )
      <*> switch
            ( long "verify"
           <> short 'v'
           <> help "verify the proof of execution for the program as well"
            )

verifyOpts :: Parser VerifyOpts
verifyOpts = VerifyOpts
         <$> strOption
               ( long "proof"
              <> short 'p'
              <> metavar "FILE"
              <> help "path to the proof to verify"
               )
         <*> strOption
               ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "path to the output of the program for the run we want to verify"
               )
         <*> strOption
               ( long "hash"
              <> short 'h'
              <> metavar "HASH"
              <> help "hash of the program"
               )
