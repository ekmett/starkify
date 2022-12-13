{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Arith where

import Data.Bits ( Bits(shiftR, shiftL) )
import Data.Int ( Int32, Int64 )
import Data.Word ( Word32, Word64 )
import GHC.Natural ( Natural )
import Test.QuickCheck
    ( Gen,
      Property,
      (===),
      ioProperty,
      Arbitrary(arbitrary),
      chooseInt,
      frequency, NonZero (..) )

import qualified Language.Wasm.Structure as W
import qualified Eval as Miden
import Validation (runValidation)
import W2M (toMASM)
import MASM.Interpreter ( fromFakeW64, FakeW64(FakeW64) )
import Data.Maybe (listToMaybe)
import Test.Hspec
import Test.Hspec.QuickCheck
import System.IO

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
    fromStack :: [Word32] -> Maybe t

instance Typed Word32 where
    typeOf _ = W32
    fromStack = listToMaybe
instance Typed Int32 where
    typeOf _ = I32
    fromStack = fmap fromIntegral . listToMaybe
instance Typed Word64 where
    typeOf _ = W64
    fromStack [hi, lo] = Just $ fromFakeW64 (FakeW64 hi lo)
    fromStack _        = Nothing
instance Typed Int64 where
    typeOf _ = I64
    fromStack = fmap fromIntegral . (fromStack :: [Word32] -> Maybe Word64)

eval :: (Integral t, Bits t, Typed t) => Expr t -> t
eval e = case e of
    ConstU32 w -> w
    ConstI32 i -> i
    ConstU64 w -> w
    ConstI64 i -> i
    Add a b -> eval a + eval b
    Sub a b -> eval a - eval b
    Mul a b -> eval a * eval b
    Div a b ->
      let ea = eval a
          eb = eval b
      in signum ea * signum eb * (abs ea `div` abs eb)
    Shl a n -> shiftL (eval a) n
    Shr a n -> shiftR (eval a) n

toWasm :: Typed t => Expr t -> W.Module
toWasm expr = W.Module
  { types = [W.FuncType [] [W.I32]]
  , tables = [], mems = [], globals = [], elems = [], datas = [], start = Just (W.StartFunction 0), imports = [], exports = []
  , functions = [ W.Function 0 [] (exprWasm expr) ]
  }

  where exprWasm :: Typed t => Expr t -> [W.Instruction Natural]
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
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IShrU
                         I32 -> W.IBinOp W.BS32 W.IShrS
                         W64 -> W.IBinOp W.BS64 W.IShrU
                         I64 -> W.IBinOp W.BS64 W.IShrS
                  xs = [W.I32Const (fromIntegral n), op]
              in exprWasm a ++ xs
            Shl a n    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IShl
                         I32 -> W.IBinOp W.BS32 W.IShl
                         W64 -> W.IBinOp W.BS64 W.IShl
                         I64 -> W.IBinOp W.BS64 W.IShl
                  xs = [W.I32Const (fromIntegral n), op]
              in exprWasm a ++ xs

randomExpr :: (Num a, Ord a, Arbitrary a) => Int -> (a -> Expr a) -> Gen (Expr a)
randomExpr 0 konst = konst <$> arbitrary
randomExpr k konst =
  frequency [ (1, randomExpr 0 konst)
            , (7, Add <$> randomExpr (k-1) konst <*> randomExpr (k-1) konst)
            , (7, Sub <$> randomExpr (k-1) konst <*> randomExpr (k-1) konst)
            , (5, Mul <$> randomExpr (k-1) konst <*> randomExpr (k-1) konst)
            , (4, Div <$> randomExpr (k-1) konst <*> (konst . getNonZero <$> arbitrary))
            , (3, Shl <$> randomExpr (k-1) konst <*> chooseInt (1, 31))
            , (2, Shr <$> randomExpr (k-1) konst <*> chooseInt (1, 31))
            ]

instance Arbitrary (Expr Word32) where
  arbitrary = randomExpr 4 ConstU32
instance Arbitrary (Expr Int32) where
  arbitrary = randomExpr 1 ConstI32
instance Arbitrary (Expr Word64) where
  arbitrary = randomExpr 4 ConstU64
instance Arbitrary (Expr Int64) where
  arbitrary = randomExpr 4 ConstI64

exprEvalCompile :: forall t. (Integral t, Bits t, Typed t, Show t) => Expr t -> Property
exprEvalCompile e = ioProperty $ do
  let reference = eval e
      wmod = toWasm e
  -- putStrLn $ "expr: " ++ show e ++ "  |   result = " ++ show reference
  mres <- runValidation (toMASM True wmod) >>= Miden.runMiden
  case mres of
    Left err -> error ("exprEvalCompile got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> t -> Property
        check midenstack val = case fromStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $
    describe "arithmetic property tests" $ do
      prop "starkify handles Word32 computations correctly" $
        exprEvalCompile @Word32
      prop "starkify handles Int32 computations correctly" $
        exprEvalCompile @Int32
      -- prop "starkify handles Word64 computations correctly" $
      --   exprEvalCompile @Word64
      -- prop "starkify handles Int64 computations correctly" $
      --   exprEvalCompile @Int64
