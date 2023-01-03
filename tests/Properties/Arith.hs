{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Arith where

import Properties.Common

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import GHC.Natural
import System.IO.Temp (emptySystemTempFile)
import Test.QuickCheck
import Text.Pretty.Simple (pShow)

import Validation (runValidation)
import W2M (toMASM)

import qualified Data.Text.Lazy.IO as T
import qualified Eval as Eval
import qualified MASM.Miden as Miden
import qualified Language.Wasm.Structure as W

data Expr t where
    ConstU32 :: Word32           -> Expr Word32
    ConstI32 :: Int32            -> Expr Int32
    ConstU64 :: Word64           -> Expr Word64
    ConstI64 :: Int64            -> Expr Int64
    Add      :: Expr a -> Expr a -> Expr a
    Sub      :: Expr a -> Expr a -> Expr a
    Mul      :: Expr a -> Expr a -> Expr a
    Div      :: Expr a -> Expr a -> Expr a
    Rem      :: Expr a -> Expr a -> Expr a
    Shl      :: Expr a -> Int    -> Expr a
    Shr      :: Expr a -> Int    -> Expr a
    Rotl     :: Expr a -> Int    -> Expr a
    Rotr     :: Expr a -> Int    -> Expr a

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
    Rem a b    -> "(" ++ show a ++ " % " ++ show b ++ ")"
    Shl a n    -> "(" ++ show a ++ " << " ++ show n ++ ")"
    Shr a n    -> "(" ++ show a ++ " >> " ++ show n ++ ")"
    Rotl a n   -> "rotl(" ++ show a ++ ", " ++ show n ++ ")"
    Rotr a n   -> "rotr(" ++ show a ++ ", " ++ show n ++ ")"

exprToWasm :: Typed t => Expr t -> W.Module
exprToWasm expr = W.Module
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
            Rem a b    ->
              let op = case typeOf a of
                         W32 -> W.IBinOp W.BS32 W.IRemU
                         I32 -> W.IBinOp W.BS32 W.IRemS
                         W64 -> W.IBinOp W.BS64 W.IRemU
                         I64 -> W.IBinOp W.BS64 W.IRemS
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
            Rotl a n   ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IRotl)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IRotl)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IRotl)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IRotl)
                  xs = [k, op]
              in exprWasm a ++ xs
            Rotr a n   ->
              let (k, op) = case typeOf a of
                         W32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IRotr)
                         I32 -> (W.I32Const (fromIntegral n), W.IBinOp W.BS32 W.IRotr)
                         W64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IRotr)
                         I64 -> (W.I64Const (fromIntegral n), W.IBinOp W.BS64 W.IRotr)
                  xs = [k, op]
              in exprWasm a ++ xs

randomExpr :: forall a. (Arbitrary a, Num a, Eq a) => (a -> Expr a) -> Int -> Gen (Expr a)
randomExpr konst 0 = konst <$> arbitrary
randomExpr konst k = frequency
  [ (1, randomExpr konst 0)
  , (7, Add <$> randomExpr konst (k-1) <*> randomExpr konst (k-1))
  , (7, Sub <$> randomExpr konst (k-1) <*> randomExpr konst (k-1))
  , (5, Mul <$> randomExpr konst (k-1) <*> randomExpr konst (k-1))
  , (4, Div <$> randomExpr konst (k-1) <*> (konst . getNonZero <$> arbitrary))
  , (3, Rem <$> randomExpr konst (k-1) <*> (konst . getNonZero <$> arbitrary))
  , (3, Shl <$> randomExpr konst (k-1) <*> chooseInt (1, 31))
  , (3, Shr <$> randomExpr konst (k-1) <*> chooseInt (1, 31))
  , (2, Rotl <$> randomExpr konst (k-1) <*> chooseInt (1, 31))
  , (2, Rotr <$> randomExpr konst (k-1) <*> chooseInt (1, 31))
  ]

logSize :: Integral a => a -> a
logSize = floor @Double . logBase 2 . fromIntegral . (+1)

instance Arbitrary (Expr Word32) where
  arbitrary = sized (randomExpr ConstU32 . logSize)
instance Arbitrary (Expr Int32) where
  arbitrary = sized (randomExpr ConstI32 . logSize)
instance Arbitrary (Expr Word64) where
  arbitrary = sized (randomExpr ConstU64 . logSize)
instance Arbitrary (Expr Int64) where
  arbitrary = sized (randomExpr ConstI64 . logSize)

arithTest :: forall t. (Integral t, Bits t, Typed t, Show t) => Bool -> Expr t -> Property
arithTest debug e = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "expr: " ++ show e ++ " ===")
  let wmod = exprToWasm e
  Just reference <- fromWStack <$> Eval.simulateWASM wmod
  when debug $ putStrLn $ "result = " ++ show reference
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ do T.putStrLn (pShow mmod)
    tmpfilename <- emptySystemTempFile "starkify-arith-test-XXX.masm"
    let keep = if debug then Miden.Keep tmpfilename else Miden.DontKeep
    res <- Eval.runMiden keep mmod
    Miden.whenKeep keep $ \fp -> putStrLn ("MASM module saved at: " ++ fp)
    return res

  when debug $ print mres
  case mres of
    Left err -> error ("arithTest got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> t -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"
