{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Control.Monad.Validate
import Control.Monad.State
import Control.Monad.RWS.Strict
import Data.DList qualified as DList
import Data.List (sortOn)
import Data.Typeable
import GHC.Natural
import GHC.Generics
import Language.Wasm.Structure qualified as W
import Language.Wasm.Validate qualified  as W

import Data.Text.Lazy qualified as LT

newtype Validation e a = Validation { getV :: ValidateT e (RWS [Ctx] () W.ResultType) a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance (Semigroup e) => MonadState W.ResultType (Validation e)
deriving instance (Semigroup e) => MonadValidate e (Validation e)
deriving instance MonadReader [Ctx] (Validation e)

bad :: e -> Validation (DList.DList (Error e)) a
bad e = do
  ctxs <- ask
  stack <- get
  refute [Error ctxs stack e]

data Ctx =
    InFunction Int -- func id
  | GlobalsInit
  | DatasInit
  | ImportsCheck
  | Typechecker
  | InInstruction Int (W.Instruction Natural) -- func id, instruction #
  | InBlock W.BlockType
  | InLoop W.BlockType
  | InIf W.BlockType
  deriving Show

inContext :: Ctx -> Validation e a -> Validation e a
inContext c = local (c:)

withContexts :: [Ctx] -> Validation e a -> Validation e a
withContexts cs = local (const cs)

data ErrorData
  = FPOperation String
  | GlobalMut W.ValueType
  | NoMain
  | StdValidation W.ValidationError
  | WasmFunctionCallIdx Int
  | UnsupportedInstruction (W.Instruction Natural)
  | Unsupported64Bits String
  | UnsupportedMemAlign Natural (W.Instruction Natural)
  | NoMultipleMem
  | UnsupportedImport LT.Text LT.Text LT.Text
  | ExpectedStack W.ParamsType
  | UnsupportedArgType W.ValueType
  | EmptyStack
  | NamedGlobalRef LT.Text
  deriving Show

data Error e = Error
  { errCtxs :: [Ctx]
  , errStack :: W.ResultType
  , errData :: e
  } deriving Show

errIdx :: ErrorData -> Int
errIdx e = case e of
  FPOperation _ -> 0
  GlobalMut _ -> 1
  NoMain -> 2
  StdValidation _ -> 3
  WasmFunctionCallIdx _ -> 4
  UnsupportedInstruction _ -> 5
  Unsupported64Bits _ -> 6
  UnsupportedMemAlign _ _ -> 7
  NoMultipleMem -> 8
  UnsupportedImport {} -> 9
  ExpectedStack _ -> 10
  EmptyStack -> 11
  UnsupportedArgType _ -> 12
  NamedGlobalRef _ -> 13

badFPOp :: String -> V a
badFPOp s = bad (FPOperation s)

badGlobalMut :: W.ValueType -> V a
badGlobalMut t = bad (GlobalMut t)

badNoMain :: V a
badNoMain = bad NoMain

badNoMultipleMem :: V a
badNoMultipleMem = bad NoMultipleMem

badImport :: W.Import -> V a
badImport (W.Import imodule iname idesc) = bad (UnsupportedImport imodule iname (descType idesc))

badNamedGlobalRef :: LT.Text -> V a
badNamedGlobalRef = bad . NamedGlobalRef

descType :: W.ImportDesc -> LT.Text
descType idesc = case idesc of
                   W.ImportFunc _ -> "function"
                   W.ImportTable _ -> "table"
                   W.ImportMemory _ -> "memory"
                   W.ImportGlobal _ -> "global"

failsStandardValidation :: W.ValidationError -> V a
failsStandardValidation e = bad (StdValidation e)

badWasmFunctionCallIdx :: Int -> V a
badWasmFunctionCallIdx i = bad (WasmFunctionCallIdx i)

unsupportedInstruction :: W.Instruction Natural -> V a
unsupportedInstruction i = bad (UnsupportedInstruction i)

unsupported64Bits :: Show op => op -> V a
unsupported64Bits op = bad (Unsupported64Bits $ show op)

unsupportedMemAlign :: Natural -> W.Instruction Natural -> V a
unsupportedMemAlign alig instr = bad (UnsupportedMemAlign alig instr)

unsupportedArgType :: W.ValueType -> V a
unsupportedArgType t = bad (UnsupportedArgType t)

ppErrData :: ErrorData -> String
ppErrData (FPOperation op) = "unsupported floating point operation: " ++ op
ppErrData (GlobalMut t) = "unsupported global mutable variable of type: " ++
  (case t of
     W.I32 -> "32 bits integer"
     W.I64 -> "64 bits integer"
     W.F32 -> "32 bits floating point"
     W.F64 -> "64 bits floating point"
  )
ppErrData NoMain = "missing main function"
ppErrData (StdValidation e) = "standard validator issue: " ++ show e
ppErrData (WasmFunctionCallIdx i) = "invalid index in function call: " ++ show i
ppErrData (UnsupportedInstruction _i) = "unsupported WASM instruction"
ppErrData (Unsupported64Bits opstr) = "unsupported 64 bit operation (" ++ opstr ++ ")"
ppErrData (UnsupportedMemAlign a _instr) = "unsupported alignment: " ++ show a
ppErrData NoMultipleMem = "multiple memories not supported"
ppErrData (UnsupportedImport imodule iname idesc) =
  "unsupported import: module=" ++ LT.unpack imodule ++
  ", name=" ++ LT.unpack iname ++ " (" ++ LT.unpack idesc ++ ")"
ppErrData (ExpectedStack expected) =
  "expected stack prefix " ++ show expected
ppErrData EmptyStack =
  "expected a non-empty stack"
ppErrData (UnsupportedArgType t) =
  "unsupported argument type: " ++ show t
ppErrData (NamedGlobalRef n) =
  "undefined global variable: " ++ show n

ppErr :: Error ErrorData -> [String]
ppErr e =
  [ red "error: " ++ ppErrData (errData e)
  ] ++
  [     "  ...  " ++ ppErrCtx c
  | c <- errCtxs e
  ] ++ [""]

  where red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"

ppErrCtx :: Ctx -> String
ppErrCtx (InFunction i) = "of function " ++ show i
ppErrCtx DatasInit = "in data section"
ppErrCtx GlobalsInit = "in globals initialisation"
ppErrCtx ImportsCheck = "in imports"
ppErrCtx Typechecker = "in typechecking"
ppErrCtx (InInstruction k i) = "in instruction #" ++ show k ++ ": " ++ take 100 (show i) ++ "  ..."
ppErrCtx (InBlock _) = "of block"
ppErrCtx (InLoop _) = "of loop"
ppErrCtx (InIf _) = "of if"

type V = Validation (DList.DList (Error ErrorData))

runValidation :: V a -> IO a
runValidation (Validation e) = case runRWS (runValidateT e) [] [] of
  (Left errs, _i, _w) -> error . unlines . ("":) $
    concatMap ppErr (sortOn (errIdx . errData) $ DList.toList errs)
  (Right a, _i, _w) -> return a
