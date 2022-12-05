module Validation where

import Control.Monad.Validate
import Control.Monad.State
import Control.Monad.RWS.Strict
import qualified Data.DList as DList
import Data.List (sortOn)
import Data.Typeable
import GHC.Natural
import GHC.Generics
import Language.Wasm.Structure

import W2M.Stack (StackProblem(..))

import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate  as WASM
import qualified Data.Text.Lazy as LT

type Id = Int

newtype Validation e a = Validation { getV :: ValidateT e (RWS [Ctx] () Id) a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance (Semigroup e) => MonadState Id (Validation e)
deriving instance (Semigroup e) => MonadValidate e (Validation e)
deriving instance MonadReader [Ctx] (Validation e)

id0 :: Id
id0 = 0

nextId :: (Semigroup e) => Validation e Id
nextId = state $ \i -> (i, i+1)

bad :: e -> Validation (DList.DList (Error e)) a
bad e = do
  ctxs <- ask
  refute (DList.singleton $ Error ctxs e)

data Ctx =
    InFunction (Maybe LT.Text) Natural -- func name, func id
  | GlobalsInit
  | DatasInit
  | ImportsCheck
  | Typechecker
  | InInstruction Int (Instruction Natural)
  | InBlock
  | InLoop
  deriving Show

inContext :: Ctx -> Validation e a -> Validation e a
inContext c m = local (c:) m

withContexts :: [Ctx] -> Validation e a -> Validation e a
withContexts cs m = local (const cs) m

data ErrorData
  = FPOperation String
  | GlobalMut ValueType
  | NoMain
  | StdValidation WASM.ValidationError
  | WasmFunctionCallIdx Int
  | UnsupportedInstruction (WASM.Instruction Natural)
  | Unsupported64Bits String
  | UnsupportedMemAlign Natural (WASM.Instruction Natural)
  | NoMultipleMem
  | UnsupportedImport LT.Text LT.Text LT.Text
  | WasmStackProblem (StackProblem [Ctx])
  | UnsupportedArgType WASM.ValueType
  deriving Show

data Error e = Error
  { errCtxs :: [Ctx]
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
  WasmStackProblem _ -> 10
  UnsupportedArgType _ -> 11

badFPOp :: String -> V a
badFPOp s = bad (FPOperation s)

badGlobalMut :: ValueType -> V a
badGlobalMut t = bad (GlobalMut t)

badNoMain :: V a
badNoMain = bad NoMain

badNoMultipleMem :: V a
badNoMultipleMem = bad NoMultipleMem

badImport :: LT.Text -> LT.Text -> LT.Text -> V a
badImport imodule iname idesc = bad (UnsupportedImport imodule iname idesc)

failsStandardValidation :: WASM.ValidationError -> V a
failsStandardValidation e = bad (StdValidation e)

badWasmFunctionCallIdx :: Int -> V a
badWasmFunctionCallIdx i = bad (WasmFunctionCallIdx i)

unsupportedInstruction :: WASM.Instruction Natural -> V a
unsupportedInstruction i = bad (UnsupportedInstruction i)

unsupported64Bits :: Show op => op -> V a
unsupported64Bits op = bad (Unsupported64Bits $ show op)

unsupportedMemAlign :: Natural -> WASM.Instruction Natural -> V a
unsupportedMemAlign alig instr = bad (UnsupportedMemAlign alig instr)

badStackTypeError :: StackProblem [Ctx] -> V a
badStackTypeError e = bad (WasmStackProblem e)

unsupportedArgType :: WASM.ValueType -> V a
unsupportedArgType t = bad (UnsupportedArgType t)

ppErrData :: ErrorData -> String
ppErrData (FPOperation op) = "unsupported floating point operation: " ++ op
ppErrData (GlobalMut t) = "unsupported global mutable variable of type: " ++
  (case t of
     I32 -> "32 bits integer"
     I64 -> "64 bits integer"
     F32 -> "32 bits floating point"
     F64 -> "64 bits floating point"
  )
ppErrData NoMain = "missing main function"
ppErrData (StdValidation e) = "standard validator issue: " ++ show e
ppErrData (WasmFunctionCallIdx i) = "invalid index in function call: " ++ show i
ppErrData (UnsupportedInstruction _i) = "unsupported WASM instruction"
ppErrData (Unsupported64Bits opstr) = "unsupported 64 bits operation (" ++ opstr ++ ")"
ppErrData (UnsupportedMemAlign a _instr) = "unsupported alignment: " ++ show a
ppErrData NoMultipleMem = "multiple memories not supported"
ppErrData (UnsupportedImport imodule iname idesc) =
  "unsupported import: module=" ++ LT.unpack imodule ++
  ", name=" ++ LT.unpack iname ++ " (" ++ LT.unpack idesc ++ ")"
ppErrData (WasmStackProblem (StackExpectedGot expected got _)) =
  "stack problem: expected stack prefix " ++ show expected ++
  " but got stack " ++ show (take (length expected) got)
ppErrData (WasmStackProblem (StackEmpty _)) =
  "stack problem: expected non empty stack"
ppErrData (UnsupportedArgType t) =
  "unsupported argument type: " ++ show t

ppErr :: Error ErrorData -> [String]
ppErr e =
  [ red "error: " ++ ppErrData (errData e)
  ] ++
  [     "  ...  " ++ ppErrCtx c
  | c <- errCtxs e
  ] ++ [""]

  where red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"

ppErrCtx :: Ctx -> String
ppErrCtx (InFunction mname i) = "of function " ++ show i ++ maybe "" (\name -> " (" ++ LT.unpack name ++ ")") mname
ppErrCtx DatasInit = "in data section"
ppErrCtx GlobalsInit = "in globals initilisation"
ppErrCtx ImportsCheck = "in imports"
ppErrCtx Typechecker = "in typechecking"
ppErrCtx (InInstruction k i) = "in instruction #" ++ show k ++ ": " ++ take 100 (show i) ++ "  ..."
ppErrCtx InBlock = "of block"
ppErrCtx InLoop = "of loop"

type V = Validation (DList.DList (Error ErrorData))

runValidation :: V a -> IO a
runValidation (Validation e) = case runRWS (runValidateT e) [] id0 of
  (Left errs, _i, _w) -> error . unlines . ("":) $
    concatMap ppErr (sortOn (errIdx . errData) $ DList.toList errs)
  (Right a, _i, _w) -> return a
