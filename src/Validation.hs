module Validation where

import Control.Monad.Validate
import Control.Monad.State
import Control.Monad.RWS.Strict
import qualified Data.DList as DList
import Data.Typeable
import GHC.Natural
import GHC.Generics

import Language.Wasm.Structure

import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate  as WASM
import qualified Data.Text.Lazy as LT

type Id = Int

newtype Validation e a = Validation { getV :: ValidateT e (RWS () () Id) a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance (Semigroup e) => MonadState Id (Validation e)
deriving instance (Semigroup e) => MonadValidate e (Validation e)

id0 :: Id
id0 = 0

nextId :: (Semigroup e) => Validation e Id
nextId = state $ \i -> (i, i+1)

bad :: e -> Validation (DList.DList e) a
bad = refute . pure

data VError
  = FPOperation String
  | GlobalMut ValueType
  | NoMain
  | StdValidation WASM.ValidationError
  | WasmFunctionCallIdx Int
  | UnsupportedInstruction (WASM.Instruction Natural)
  | Unsupported64Bits String
  | UnsupportedMemAlign Natural
  | NoMultipleMem
  | UnsupportedImport LT.Text LT.Text LT.Text
  deriving Show

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

unsupportedMemAlign :: Natural -> V a
unsupportedMemAlign alig = bad (UnsupportedMemAlign alig)

ppErr :: VError -> String
ppErr (FPOperation op) = "a floating point operation: " ++ op
ppErr (GlobalMut t) = "a global mutable variable of type: " ++
  (case t of
     I32 -> "32 bits integer"
     I64 -> "64 bits integer"
     F32 -> "32 bits floating point"
     F64 -> "64 bits floating point"
  )
ppErr NoMain = "missing main function"
ppErr (StdValidation e) = "a standard validator issue: " ++ show e
ppErr (WasmFunctionCallIdx i) = "an invalid index in function call: " ++ show i
ppErr (UnsupportedInstruction i) = "an unsupported WASM instruction: " ++ show i
ppErr (Unsupported64Bits opstr) = "a 64 bits operation (" ++ opstr ++ ")"
ppErr (UnsupportedMemAlign a) = "an unsupported alignment: " ++ show a
ppErr NoMultipleMem = "a need for multiple memories"
ppErr (UnsupportedImport imodule iname idesc) =
  "an unsupported import: module=" ++ LT.unpack imodule ++
  ", name=" ++ LT.unpack iname ++ " (" ++ LT.unpack idesc ++ ")"

type V = Validation (DList.DList VError)

runValidation :: V a -> IO a
runValidation (Validation e) = case runRWS (runValidateT e) () id0 of
  (Left errs, _i, _w) -> error . unlines $
    "found: " : fmap (\err -> " - " ++ ppErr err) (DList.toList errs)
  (Right a, _i, _w) -> return a
