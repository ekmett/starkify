module Properties.Common where

import Data.Int
import Data.Maybe
import Data.Word
import Debug.Trace

import MASM.Interpreter

import qualified Language.Wasm.Interpreter as W
import qualified Language.Wasm.Structure as W

tr :: Show a => String -> a -> a
tr lbl a = trace ("[TRACE] " ++ lbl ++ ": " ++ show a) a

data Ty = W32 | I32 | W64 | I64

class Typed t where
    typeOf :: e t -> Ty
    fromMStack :: [Word32] -> Maybe t
    fromWStack :: [W.Value] -> Maybe t
    wasmPush   :: t -> W.Instruction i
    bitsize    :: t -> W.BitSize
    isSignedTy :: t -> Bool

instance Typed Word32 where
    typeOf _ = W32
    fromMStack = listToMaybe
    fromWStack s = case s of
      W.VI32 w : _ -> Just w
      _            -> Nothing
    wasmPush w = W.I32Const w
    bitsize _ = W.BS32
    isSignedTy _ = False
instance Typed Int32 where
    typeOf _ = I32
    fromMStack = fmap fromIntegral . listToMaybe
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word32)
    wasmPush i = W.I32Const (fromIntegral i)
    bitsize _ = W.BS32
    isSignedTy _ = True
instance Typed Word64 where
    typeOf _ = W64
    fromMStack (hi:lo:_) = Just $ fromFakeW64 (FakeW64 hi lo)
    fromMStack _         = Nothing
    fromWStack s = case s of
      W.VI64 w : _ -> Just w
      _            -> Nothing
    wasmPush w = W.I64Const w
    bitsize _ = W.BS64
    isSignedTy _ = False
instance Typed Int64 where
    typeOf _ = I64
    fromMStack = fmap fromIntegral . (fromMStack :: [Word32] -> Maybe Word64)
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word64)
    wasmPush w = W.I64Const (fromIntegral w)
    bitsize _ = W.BS64
    isSignedTy _ = True

