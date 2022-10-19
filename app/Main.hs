{-# LANGUAGE LambdaCase #-}
module Main where

import Validation
import MASM (ppMASM)
import W2M

import Data.Foldable
import Language.Wasm
import Language.Wasm.Structure
import System.Environment

import qualified Data.ByteString.Lazy as LBS
import qualified Language.Wasm.Validate as WASM

import Debug.Trace

main :: IO ()
main = getArgs >>= \case
  ["bin", fp] -> runBin fp
  ["txt", fp] -> runTxt fp
  _    -> usage

usage :: IO ()
usage = error "This program expects the path to a WASM bytecode file as an argument."

runTxt :: FilePath -> IO ()
runTxt fp = run fp parse

runBin :: FilePath -> IO ()
runBin fp = run fp decodeLazy

run :: Show err => FilePath -> (LBS.ByteString -> Either err Module) -> IO ()
run fp dec = do
  r <- dec <$> LBS.readFile fp
  case r of
    Left e -> error $ "Decoding error: " ++ show e
    Right wasm_mod -> do
      masm_mod <- runValidation $ do
            standardValidator wasm_mod
            traceShow wasm_mod (analyze wasm_mod)
            toMASM wasm_mod
      putStrLn (ppMASM masm_mod)

  where standardValidator wasm_mod = do
          case WASM.validate wasm_mod of
            Left err -> failsStandardValidation err
            Right _validMod -> return ()

-- TODO: run "typechecker" (validator) from 'wasm' first?
analyze :: Module -> V ()
analyze m = sequenceA_
  [ analyzeFunctions (functions m)

  -- TODO: this complains about a global mutable var of type int32 in
  --       a minimal C program's WASM, we need to make it finer grained and
  --       allow compiler-generated things like this.
  -- , analyzeGlobals   (globals mod)
  ]

analyzeFunctions :: [Function] -> V ()
analyzeFunctions = traverse_ analyzeFunction

analyzeFunction :: Function -> V ()
analyzeFunction (Function _ _ b) = analyzeInstructions b

analyzeInstructions :: [Instruction a] -> V ()
analyzeInstructions = traverse_ analyzeInstruction

analyzeInstruction :: Instruction a -> V ()
analyzeInstruction = \case
  Block _ b -> analyzeInstructions b
  Loop _ b -> analyzeInstructions b
  If _ t f -> analyzeInstructions (t ++ f)
  FUnOp _bitsz funop -> analyzeFUnop funop
  FBinOp _bitsz fbinop -> analyzeFBinop fbinop
  FRelOp _bitsz frelop -> analyzeFRelop frelop
  _ -> ok

  where ok = pure ()

analyzeFUnop :: FUnOp -> V ()
analyzeFUnop unop = badFPOp $ case unop of
  FAbs  -> "abs"
  FNeg  -> "negate"
  FCeil -> "ceiling"
  FFloor -> "floor"
  FTrunc -> "truncate"
  FNearest -> "nearest"
  FSqrt -> "sqrt"

analyzeFBinop :: FBinOp -> V ()
analyzeFBinop binop = badFPOp $ case binop of
  FAdd -> "+"
  FSub -> "-"
  FMul -> "*"
  FDiv -> "/"
  FMin -> "min"
  FMax -> "max"
  FCopySign -> "copysign"

analyzeFRelop :: FRelOp -> V ()
analyzeFRelop relop = badFPOp $ case relop of
  FEq -> "=="
  FNe -> "!="
  FLt -> "<"
  FGt -> ">"
  FLe -> "<="
  FGe -> ">="

analyzeGlobals :: [Global] -> V ()
analyzeGlobals = traverse_ analyzeGlobal

analyzeGlobal :: Global -> V ()
analyzeGlobal (Global t b) = sequenceA_
  [ analyzeGlobalType t
  , analyzeInstructions b
  ]

analyzeGlobalType :: GlobalType -> V ()
analyzeGlobalType = \case
  Mut valuet -> badGlobalMut valuet
  _ -> pure ()
