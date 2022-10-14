module Validation where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import GHC.Natural
import Language.Wasm.Structure

import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate  as WASM

type Id = Int

newtype Validation e a = Validation { getV :: ExceptT e (State Id) a }

id0 :: Id
id0 = 0

nextId :: Validation e Id
nextId = Validation $ ExceptT $ StateT $ \i ->
  Identity (Right i, i+1)

instance Functor (Validation e) where
  fmap f (Validation e) = Validation (fmap f e)

instance Semigroup e => Applicative (Validation e) where
  pure a = Validation (pure a)
  Validation ef <*> Validation ex = Validation $ ExceptT $ StateT $ \i ->
    case runState (runExceptT ef) i of
      (res_f, i') -> case runState (runExceptT ex) i' of
        (res_x, i'') -> Identity $ case (res_f, res_x) of
          (Left e , Left e') -> (Left (e <> e'), i'')
          (Left e , _      ) -> (Left e, i'')
          (_      , Left e') -> (Left e', i'')
          (Right f, Right x) -> (Right (f x), i'')

instance Semigroup e => Monad (Validation e) where
  Validation m >>= f = Validation $ ExceptT $ StateT $ \i ->
    case runState (runExceptT m) i of
      (Left e, i') -> Identity (Left e, i')
      (Right a, i') -> Identity $ runState (runExceptT . getV $ f a) i'

bad :: e -> Validation [e] a
bad e = Validation $ ExceptT $ StateT $ \s -> Identity (Left [e], s)

data VError
  = FPOperation String
  | GlobalMut ValueType
  | NoMain
  | StdValidation WASM.ValidationError
  | WasmFunctionCallIdx Int
  | UnsupportedInstruction (WASM.Instruction Natural)
  | Unsupported64Bits String
  deriving Show

badFPOp :: String -> V a
badFPOp s = bad (FPOperation s)

badGlobalMut :: ValueType -> V a
badGlobalMut t = bad (GlobalMut t)

badNoMain :: V a
badNoMain = bad NoMain

failsStandardValidation :: WASM.ValidationError -> V a
failsStandardValidation e = bad (StdValidation e)

badWasmFunctionCallIdx :: Int -> V a
badWasmFunctionCallIdx i = bad (WasmFunctionCallIdx i)

unsupportedInstruction :: WASM.Instruction Natural -> V a
unsupportedInstruction i = bad (UnsupportedInstruction i)

unsupported64Bits :: Show op => op -> V a
unsupported64Bits op = bad (Unsupported64Bits $ show op)

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

type V = Validation [VError]

runValidation :: V a -> IO a
runValidation (Validation e) = case runState (runExceptT e) id0 of
  (Left errs, _i) -> error . unlines $
    "found: " : map (\err -> " - " ++ ppErr err) errs
  (Right a, _i) -> return a
