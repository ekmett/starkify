{-# LANGUAGE OverloadedLists #-}
module Validation where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS.Strict
import qualified Data.DList as DList
import Data.Typeable
import GHC.Natural
import qualified GHC.Exts
import GHC.Generics

import Language.Wasm.Structure

import qualified Language.Wasm.Structure as WASM
import qualified Language.Wasm.Validate  as WASM

type Id = Int

newtype Validation e a = Validation { getV :: ExceptT e (RWS () () Id) a }
  deriving (Generic, Typeable, Functor) -- , Applicative, Monad)

deriving instance (Semigroup e) => MonadState Id (Validation e)
deriving instance (Semigroup e) => MonadError e (Validation e)

id0 :: Id
id0 = 0

nextId :: (Semigroup e) => Validation e Id
nextId = state $ \i -> (i, i+1)

instance Semigroup e => Applicative (Validation e) where
  pure a = Validation (pure a)
  Validation ef <*> Validation ex = Validation $ ExceptT $ rws $ \_ i ->
    case runRWS (runExceptT ef) () i of
      (res_f, i', _) -> case runRWS (runExceptT ex) () i' of
        (res_x, i'', _) -> case (res_f, res_x) of
          (Left e , Left e') -> (Left (e <> e'), i'', ())
          (Left e , _      ) -> (Left e, i'', ())
          (_      , Left e') -> (Left e', i'', ())
          (Right f, Right x) -> (Right (f x), i'', ())

instance Semigroup e => Monad (Validation e) where
  Validation m >>= f = Validation $ ExceptT $ rws $ \_ i ->
    case runRWS (runExceptT m) () i of
      (Left e, i', _) -> (Left e, i', ())
      (Right a, i', _) -> runRWS (runExceptT . getV $ f a) () i'

bad :: e -> Validation (DList.DList e) a
bad e = Validation $ ExceptT $ rws $ \_ s -> (Left [e], s, ())

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

type V = Validation (DList.DList VError)

runValidation :: V a -> IO a
runValidation (Validation e) = case runRWS (runExceptT e) () id0 of
  (Left errs, _i, _w) -> error . unlines $
    "found: " : fmap (\err -> " - " ++ ppErr err) (DList.toList errs)
  (Right a, _i, _w) -> return a
