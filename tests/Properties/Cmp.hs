{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Properties.Cmp where
import Test.QuickCheck
import qualified Language.Wasm.Structure as W
import Properties.Common
import Data.Word
import qualified Eval
import qualified MASM.Miden as Miden
import qualified Data.Text.Lazy.IO as T
import Text.Pretty.Simple
import W2M (toMASM)
import Validation (runValidation)
import Control.Monad
import System.IO.Temp

data CmpOp = Lt | Gt | Lte | Gte | Eq | Neq
  deriving (Eq, Show, Enum, Bounded)

prettyCmpOp :: CmpOp -> String
prettyCmpOp o = case o of
  Lt  -> "<"
  Gt  -> ">"
  Lte -> "<="
  Gte -> ">="
  Eq  -> "=="
  Neq -> "/="

allCmpOps :: [CmpOp]
allCmpOps = [minBound .. maxBound]

instance Arbitrary CmpOp where
  arbitrary = elements allCmpOps

runCmpOp :: Ord a => CmpOp -> a -> a -> Bool
runCmpOp op = opFun
  where opFun = case op of
          Lt  -> (<)
          Gt  -> (>)
          Lte -> (<=)
          Gte -> (>=)
          Eq  -> (==)
          Neq -> (/=)

data Cmp a = Cmp a a CmpOp

instance Show a => Show (Cmp a) where
  show (Cmp a b op) = unwords [show a, prettyCmpOp op, show b]

instance Arbitrary a => Arbitrary (Cmp a) where
  arbitrary = Cmp <$> arbitrary <*> arbitrary <*> arbitrary

cmpToWasm :: Typed a => Cmp a -> W.Module
cmpToWasm (Cmp a b op) = W.Module
  { types = [W.FuncType [] [W.I32]]
  , start = Nothing
  , exports = [ W.Export "main" (W.ExportFunc 0) ]
  , functions = [ W.Function 0 [] instrs ]
  , tables = [], mems = [], globals = [], elems = [], datas = [], imports = []
  }
  where instrs = [ wasmPush a, wasmPush b, W.IRelOp sz relop ]
        sz = bitsize a
        signedTy = isSignedTy a
        relop = case op of
          Lt  -> if signedTy then W.ILtS else W.ILtU
          Gt  -> if signedTy then W.IGtS else W.IGtU
          Lte -> if signedTy then W.ILeS else W.ILeU
          Gte -> if signedTy then W.IGeS else W.IGeU
          Eq  -> W.IEq
          Neq -> W.INe

cmpTest :: forall a. (Arbitrary a, Eq a, Show a, Typed a) => Bool -> Cmp a -> Property
cmpTest debug cmp@(Cmp x y cmpop) = ioProperty $ do
  when debug $ putStrLn ("\n\n=== " ++ "expr: " ++ show x ++ " " ++ prettyCmpOp cmpop ++ " " ++ show y ++ " ===")
  let wmod = cmpToWasm cmp
  Just reference <- fromWStack @Word32 <$> Eval.simulateWASM wmod
  when debug $
    putStrLn $ "result = " ++ show reference ++ " (" ++
               show (reference == 1) ++ ")"
  when debug $ T.putStrLn (pShow wmod)
  mres <- runValidation (toMASM wmod) >>= \mmod -> do
    when debug $ T.putStrLn (pShow mmod)
    tmpfilename <- emptySystemTempFile "starkify-cmp-test-XXX.masm"
    let keep = if debug then Miden.Keep tmpfilename else Miden.DontKeep
    res <- Eval.runMiden keep mmod
    Miden.whenKeep keep $ \fp -> putStrLn ("MASM module saved at: " ++ fp)
    return res
  when debug $ print mres
  case mres of
    Left err -> error ("cmpTest got a miden error: " ++ err)
    Right res -> return $ check res reference

  where check :: [Word32] -> Word32 -> Property
        check midenstack val = case fromMStack midenstack of
          Just res -> val === res
          Nothing -> error "couldn't extract result"
