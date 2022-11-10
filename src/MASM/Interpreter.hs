module MASM.Interpreter where

import Data.Bits ( Bits(shiftR, shiftL), (.&.), xor, (.|.) )
import Data.Word
import MASM
import qualified Data.Text.Lazy as T
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.IntMap.Strict as IntMap

type Value = Word32
type Stack = [Value]

data Mem = Mem
  { locals :: IntMap.IntMap Word32
  , linearmem :: IntMap.IntMap Word32
  } deriving Show

interpret :: Module -> (Stack, Mem)
interpret m = go (programInstrs $ moduleProg m) stack0 mem0

  where stack0 = replicate 16 0
        mem0 = Mem IntMap.empty IntMap.empty

        swapHead _ [] = []
        swapHead i (x:xs) = case f 1 xs of
            (a, ys) -> a:ys
          where f k (a:as)
                  | k == i = (a, x:as)
                  | otherwise = case f (k+1) as of
                      (v, vs) -> (v, a:vs)
                f _ [] = error "swapHead: invalid input"

        go (Push w : is) st mem = go is (w:st) mem
        go (Drop : is) (_x:xs) mem = go is xs mem
        go (Dup k : is) st mem
          | fromIntegral k >= length st = impossible (Dup k) st mem
          | otherwise = let e = st !! fromIntegral k in
             go is (e:st) mem
        go (Swap k : is) st mem
          | fromIntegral k >= length st = impossible (Swap 1) st mem
          | otherwise = go is (swapHead k st) mem
        go (IAdd : is) (b:a:xs) mem = go is ((a+b):xs) mem
        go (IAdd64 : is) (b_hi:b_lo:a_hi:a_lo:xs) mem =
            let a = fromFakeW64 (FakeW64 a_hi a_lo)
                b = fromFakeW64 (FakeW64 b_hi b_lo)
                c = a + b
                FakeW64 c_hi c_lo = toFakeW64 c
            in go is (c_hi:c_lo:xs) mem
        go (IMul64 : is) (b_hi:b_lo:a_hi:a_lo:xs) mem =
            let a = fromFakeW64 (FakeW64 a_hi a_lo)
                b = fromFakeW64 (FakeW64 b_hi b_lo)
                c = a * b
                FakeW64 c_hi c_lo = toFakeW64 c
            in go is (c_hi:c_lo:xs) mem
        go (ISub : is) (b:a:xs) mem = go is ((a-b):xs) mem
        go (IMul : is) (b:a:xs) mem = go is ((a*b):xs) mem
        go (IDiv : is) (b:a:xs) mem = go is ((a`div`b):xs) mem
        go (ShL : is) (b:a:xs) mem = go is ((shiftL a (fromIntegral b)):xs) mem
        go (ShR : is) (b:a:xs) mem = go is ((shiftR a (fromIntegral b)):xs) mem
        go (And : is) (b:a:xs) mem = go is ((a .&. b):xs) mem
        go (Or : is) (b:a:xs) mem = go is ((a .|. b):xs) mem
        go (Xor : is) (b:a:xs) mem = go is ((a `xor` b):xs) mem
        go (EqConst w : is) (a:xs) mem = go is ((if a == w then 1 else 0):xs) mem
        go (Eq : is) (b:a:xs) mem = go is ((if a == b then 1 else 0):xs) mem
        go (Neq : is) (b:a:xs) mem = go is ((if a /= b then 1 else 0):xs) mem
        go (Lt : is) (b:a:xs) mem = go is ((if a < b then 1 else 0):xs) mem
        go (Gt : is) (b:a:xs) mem = go is ((if a > b then 1 else 0):xs) mem
        go (TruncateStack : is) st mem = go is st mem
        go (Exec procedure : is) st mem = case findProc procedure of
            -- check nlocals & stack compatibility?
            Just p -> case go (procInstrs p) st mem of
                (st', mem') -> go is st' (mem' { locals = locals mem })
            Nothing -> error $ "Unknown procedure `" ++ T.unpack procedure ++ "`"
        go (LocStore w : is) (a:xs) mem = go is xs (mem { locals = IntMap.insert (fromIntegral w) a (locals mem) })
        go (LocLoad w : is) st mem =
            let a = fromMaybe 0 $ IntMap.lookup (fromIntegral w) (locals mem)
            in go is (a:st) mem
        go (MemLoad Nothing : is) (addr:xs) mem =
            let a = fromMaybe 0 $ IntMap.lookup (fromIntegral addr) (linearmem mem)
            in go is (a:xs) mem
        go (MemLoad (Just addr) : is) st mem =
            let a = fromMaybe 0 $ IntMap.lookup (fromIntegral addr) (linearmem mem)
            in go is (a:st) mem
        go (MemStore Nothing : is) (addr:v:xs) mem =
            go is (v:xs) (mem { linearmem = IntMap.insert (fromIntegral addr) v (linearmem mem) })
        go (MemStore (Just addr) : is) (v:xs) mem =
            go is (v:xs) (mem { linearmem = IntMap.insert (fromIntegral addr) v (linearmem mem) })
        go (IfTrue thenB elseB : is) (a:xs) mem =
            case a of
                1 -> go (thenB ++ is) xs mem
                0 -> go (elseB ++ is) xs mem
                _ -> impossible (IfTrue thenB elseB) (a:xs) mem
        go (i:_) st mem = impossible i st mem
        go [] st mem = (st, mem)

        findProc name = listToMaybe $ filter (\p -> procName p == name) (moduleProcs m)
        impossible inst st mem = error $ unlines
          [ "Cannot execute `" ++ show inst ++ "`"
          , "  with stack: " ++ show st
          , "  and memory: " ++ show mem
          ]

data FakeW64 = FakeW64 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
  deriving Show

toFakeW64 :: Word64 -> FakeW64
toFakeW64 w = FakeW64 w_hi w_lo
  where w_hi = fromIntegral $ shiftR (w .&. 0xFFFFFFFF00000000) 32
        w_lo = fromIntegral $         w .&. 0x00000000FFFFFFFF
fromFakeW64 :: FakeW64 -> Word64
fromFakeW64 (FakeW64 hi lo) = shiftL (fromIntegral hi) 32 .|. fromIntegral lo
