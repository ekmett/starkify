{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Arith where

import Data.Bits ( Bits(..) )
import Data.Int ( Int32, Int64 )
import Data.Maybe (listToMaybe)
import Data.Word ( Word32, Word64 )
import GHC.Natural ( Natural )
import System.IO ( hSetBuffering, stdout, BufferMode(..) )
import Test.Hspec ( hspec, describe )
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck
    ( Gen,
      Property,
      (===),
      ioProperty,
      Arbitrary(arbitrary),
      chooseInt,
      frequency, NonZero (..) )

import qualified Language.Wasm.Interpreter as W
import qualified Language.Wasm.Structure as W
import qualified Data.Text.Lazy.IO as T

import qualified Eval as Miden
import Validation (runValidation)
import W2M (toMASM)
import MASM.Interpreter ( fromFakeW64, FakeW64(FakeW64) )
import Text.Pretty.Simple

data Expr t where
    ConstU32 :: Word32           -> Expr Word32
    ConstI32 :: Int32            -> Expr Int32
    ConstU64 :: Word64           -> Expr Word64
    ConstI64 :: Int64            -> Expr Int64
    Add      :: Expr a -> Expr a -> Expr a
    Sub      :: Expr a -> Expr a -> Expr a
    Mul      :: Expr a -> Expr a -> Expr a
    Div      :: Expr a -> Expr a -> Expr a
    Shl      :: Expr a -> Int    -> Expr a
    Shr      :: Expr a -> Int    -> Expr a

instance Show (Expr t) where
  show e = case e of
    ConstU32 w -> show w
    ConstI32 w -> show w
    ConstU64 w -> show w
    ConstI64 w -> show w
    Add a b    -> "(" ++ show a ++ " + " ++ show b ++ ")"
    Sub a b    -> "(" ++ show a ++ " - " ++ show b ++ ")"
    Mul a b    -> "(" ++ show a ++ " * " ++ show b ++ ")"
    Div a b    -> "(" ++ show a ++ " / " ++ show b ++ ")"
    Shl a n    -> "(" ++ show a ++ " << " ++ show n ++ ")"
    Shr a n    -> "(" ++ show a ++ " >> " ++ show n ++ ")"

data Ty = W32 | I32 | W64 | I64

class Typed t where
    typeOf :: Expr t -> Ty
    fromMStack :: [Word32] -> Maybe t
    fromWStack :: [W.Value] -> Maybe t

instance Typed Word32 where
    typeOf _ = W32
    fromMStack = listToMaybe
    fromWStack s = case s of
      W.VI32 w : _ -> Just w
      _            -> Nothing
instance Typed Int32 where
    typeOf _ = I32
    fromMStack = fmap fromIntegral . listToMaybe
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word32)
instance Typed Word64 where
    typeOf _ = W64
    fromMStack (hi:lo:_) = Just $ fromFakeW64 (FakeW64 hi lo)
    fromMStack _         = Nothing
    fromWStack s = case s of
      W.VI64 w : _ -> Just w
      _            -> Nothing
instance Typed Int64 where
    typeOf _ = I64
    fromMStack = fmap fromIntegral . (fromMStack :: [Word32] -> Maybe Word64)
    fromWStack = fmap fromIntegral . (fromWStack :: [W.Value] -> Maybe Word64)

toWasm :: Typed t => Expr t -> W.Module
toWasm expr = W.Module
  { types = [W.FuncType [] [retty]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] (exprWasm expr) ]
  , tables = [], mems = [], globals = [], elems = [], datas = [], imports = []
  }

  where retty = case typeOf expr of
          W32 -> W.I32
          I32 -> W.I32
          W64 -> W.I64
          I64 -> W.I64

        exprWasm :: Typed t => Expr t -> [W.Instruction Natural]
        exprWasm e = case e of
            ConstU32 w -> [W.I32Const w]
            ConstI32 w -> [W.I32Const (fromIntegral w)]
            ConstU64 w -> [W.I64Const w]
            ConstI64 w -> [W.I64Const (fromIntegral w)]
            Add a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IAdd
                         I32 -> W.IBinOp W.BS32 W.IAdd
                         W64 -> W.IBinOp W.BS64 W.IAdd
                         I64 -> W.IBinOp W.BS64 W.IAdd
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Sub a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.ISub
                         I32 -> W.IBinOp W.BS32 W.ISub
                         W64 -> W.IBinOp W.BS64 W.ISub
                         I64 -> W.IBinOp W.BS64 W.ISub
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Mul a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IMul
                         I32 -> W.IBinOp W.BS32 W.IMul
                         W64 -> W.IBinOp W.BS64 W.IMul
                         I64 -> W.IBinOp W.BS64 W.IMul
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Div a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IDivU
                         I32 -> W.IBinOp W.BS32 W.IDivS
                         W64 -> W.IBinOp W.BS64 W.IDivU
                         I64 -> W.IBinOp W.BS64 W.IDivS
                  xs = [op]
              in exprWasm a ++ exprWasm b ++ xs
            Shr a n    ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShrU)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShrS)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShrU)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShrS)
                  xs = [k, op]
              in exprWasm a ++ xs
            Shl a n    ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShl)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IShl)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShl)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IShl)
                  xs = [k, op]
              in exprWasm a ++ xs

randomExpr :: forall a. (Arbitrary a, Num a, Eq a) => Int -> (a -> Expr a) -> Maybe Bool -> Gen (Expr a)
randomExpr 0 konst _ = konst <$> arbitrary
randomExpr k konst mwithShrDiv = frequency $
  [ (1, randomExpr 0 konst mwithShrDiv)
  , (7, Add <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (7, Sub <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (5, Mul <$> randomExpr (k-1) konst mwithShrDiv <*> randomExpr (k-1) konst mwithShrDiv)
  , (3, Shl <$> randomExpr (k-1) konst mwithShrDiv <*> chooseInt (1, 31))
  ] ++
  case mwithShrDiv of
    Just True ->
      [ (4, Div <$> randomExpr (k-1) konst mwithShrDiv <*> (konst . getNonZero <$> arbitrary))
      , (2, Shr <$> randomExpr (k-1) konst mwithShrDiv <*> chooseInt (1, 31))
      ]
    Just False ->
      [ (4, Div <$> randomExpr 0 konst mwithShrDiv <*> (konst . getNonZero <$> arbitrary))
      , (2, Shr <$> randomExpr 0 konst mwithShrDiv <*> chooseInt (1, 3))
      ] -- hopefully those more modest generators are enough to avoid the annoying corner cases...
    Nothing -> [ (2, Shr <$> randomExpr 0 konst mwithShrDiv <*> chooseInt (1, 3)) ]

exprDepth :: Int
exprDepth = 4

instance Arbitrary (Expr Word32) where
  arbitrary = randomExpr exprDepth ConstU32 (Just True)
instance Arbitrary (Expr Int32) where
  -- TODO: Turn 'False' to 'True' when our translation of signed division
  --       can handle all corner cases. Until then we generate simpler signed
  --       division/shift expressions.
  arbitrary = randomExpr exprDepth ConstI32 (Just False)
instance Arbitrary (Expr Word64) where
  arbitrary = randomExpr exprDepth ConstU64 (Just True)
instance Arbitrary (Expr Int64) where
  arbitrary = randomExpr 1 ConstI64 Nothing -- TODO: generate signed div and shr

exprEvalCompile :: forall t. (Integral t, Bits t, Typed t, Show t) => Expr t -> Property
exprEvalCompile e = ioProperty $ do
  -- putStrLn ("\n\n=== " ++ "expr: " ++ show e ++ " ===")
  let wmod = toWasm e
  -- T.putStrLn (pShow wmod)
  Just reference <- fromWStack <$> Miden.simulateWASM wmod
  -- putStrLn $ "result = " ++ show reference
  -- T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    -- T.putStrLn (pShow mmod)
    Miden.runMiden mmod
  -- print mres
  case mres of
    Left err -> error ("exprEvalCompile got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> t -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"

newtype Pow = Pow Int
  deriving Show

instance Arbitrary Pow where
  arbitrary = Pow <$> chooseInt (1, 31)

-- To run more examples, use something like
-- cabal test --test-option=--qc-max-success=10000 arith-test
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hspec $
    describe "starkify preserves arithmetic computations results" $ do
      prop "for unsigned 32 bits integers" $
        exprEvalCompile @Word32
      prop "for signed 32 bits integers" $
        exprEvalCompile @Int32
      prop "for unsigned 64 bits integers" $
        exprEvalCompile @Word64
      prop "for signed 64 bits integer" $
        exprEvalCompile @Int64
