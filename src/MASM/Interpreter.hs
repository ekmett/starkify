module MASM.Interpreter where

import Data.Bits ( Bits(shiftR, shiftL), (.&.), xor, (.|.) )
import Data.Word ( Word32, Word64 )
import MASM
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.IntMap.Strict as IntMap

type Value = Word32
type Stack = [Value]

data Mem = Mem
  { locals :: IntMap.IntMap Word32
  , linearmem :: IntMap.IntMap Word32
  } deriving Show

newtype Interp a = Interp { runInterp :: Either String a }
  deriving (Eq, Show, Functor, Applicative, Monad)

instance MonadFail Interp where
    fail = Interp . Left

interpret :: Module -> Interp (Stack, Mem)
interpret m = foldM step state0 (programInstrs $ moduleProg m)

  where stack0 = replicate 16 0
        mem0 = Mem IntMap.empty IntMap.empty
        state0 = (stack0, mem0)
        
foldM :: MonadFail m => (a -> s -> m s) -> s -> [a] -> m s
foldM f z = go z
    where go s (x:xs) = do
            s' <- f x s
            go s' xs
          go s [] = return s

step :: MonadFail m => Instruction -> (Stack, Mem) -> m (Stack, Mem)
step (Push w) (st, mem) = pure (w:st, mem)
step Drop (_x:xs, mem) = pure (xs, mem)
step (Dup k) (st, mem)
    | fromIntegral k >= length st = impossible (Dup k) st mem
    | otherwise = let e = st !! fromIntegral k in
        pure (e:st, mem)
step (Swap k) (st, mem)
    | fromIntegral k >= length st = impossible (Swap k) st mem
    | otherwise = do
        st' <- swapHead k st
        pure (st', mem)
step IAdd (b:a:xs, mem) = pure ((a+b):xs, mem)
step IAdd64 (b_hi:b_lo:a_hi:a_lo:xs, mem) =
    let a = fromFakeW64 (FakeW64 a_hi a_lo)
        b = fromFakeW64 (FakeW64 b_hi b_lo)
        c = a + b
        FakeW64 c_hi c_lo = toFakeW64 c
    in pure (c_hi:c_lo:xs, mem)
step IMul64 (b_hi:b_lo:a_hi:a_lo:xs, mem) =
    let a = fromFakeW64 (FakeW64 a_hi a_lo)
        b = fromFakeW64 (FakeW64 b_hi b_lo)
        c = a * b
        FakeW64 c_hi c_lo = toFakeW64 c
    in pure (c_hi:c_lo:xs, mem)
step ISub (b:a:xs, mem) = pure ((a-b):xs, mem)
step IMul (b:a:xs, mem) = pure ((a*b):xs, mem)
step IDiv (b:a:xs, mem) = pure ((a`div`b):xs, mem)
step IShL (b:a:xs, mem) = pure ((shiftL a (fromIntegral b)):xs, mem)
step IShR (b:a:xs, mem) = pure ((shiftR a (fromIntegral b)):xs, mem)
step IAnd (b:a:xs, mem) = pure ((a .&. b):xs, mem)
step IOr (b:a:xs, mem) = pure ((a .|. b):xs, mem)
step IXor (b:a:xs, mem) = pure ((a `xor` b):xs, mem)
step (IEq Nothing) (b:a:xs, mem) = pure ((if a == b then 1 else 0):xs, mem)
step (IEq (Just c)) (a:xs, mem) = pure ((if a == c then 1 else 0):xs, mem)
step INeq (b:a:xs, mem) = pure ((if a /= b then 1 else 0):xs, mem)
step ILt (b:a:xs, mem) = pure ((if a < b then 1 else 0):xs, mem)
step IGt (b:a:xs, mem) = pure ((if a > b then 1 else 0):xs, mem)
{-
step (Exec procedure : is) (st, mem) = case findProc procedure of
    -- check nlocals & stack compatibility?
    Just p -> case go (procInstrs p) st mem of
        (st', mem') -> go is st' (mem' { locals = locals mem })
    Nothing -> error $ "Unknown procedure `" ++ T.unpack procedure ++ "`"
-}
step (LocStore w) (a:xs, mem) = pure (xs, mem { locals = IntMap.insert (fromIntegral w) a (locals mem) })
step (LocLoad w) (st, mem) =
    let a = fromMaybe 0 $ IntMap.lookup (fromIntegral w) (locals mem)
    in pure (a:st, mem)
step (MemLoad Nothing) (addr:xs, mem) =
    let a = fromMaybe 0 $ IntMap.lookup (fromIntegral addr) (linearmem mem)
    in pure (a:xs, mem)
step (MemLoad (Just addr)) (st, mem) =
    let a = fromMaybe 0 $ IntMap.lookup (fromIntegral addr) (linearmem mem)
    in pure (a:st, mem)
step (MemStore Nothing) (addr:v:xs, mem) = pure
    (v:xs, mem { linearmem = IntMap.insert (fromIntegral addr) v (linearmem mem) })
step (MemStore (Just addr)) (v:xs, mem) = pure
    (v:xs, mem { linearmem = IntMap.insert (fromIntegral addr) v (linearmem mem) })
{-
step (IfTrue thenB elseB : is) (a:xs) mem =
    case a of
        1 -> go (thenB ++ is) xs mem
        0 -> go (elseB ++ is) xs mem
        _ -> impossible (IfTrue thenB elseB) (a:xs) mem
-}
step TruncateStack (st, mem) = pure (take 16 st, mem) -- probably not quite what Miden does...
step i (st, mem) = impossible i st mem

findProc :: ProcName -> Module -> Maybe Proc
findProc name m = listToMaybe $ filter (\p -> procName p == name) (moduleProcs m)

impossible :: MonadFail m => Instruction -> [Value] -> Mem -> m a
impossible inst st mem = fail $ unlines
    [ "Cannot execute `" ++ show inst ++ "`"
    , "  with stack: " ++ show st
    , "  and memory: " ++ show mem
    ]

swapHead :: MonadFail m => Word32 -> Stack -> m Stack
swapHead _ [] = pure []
swapHead i (x:xs) = do
    r <- f 1 xs
    case r of
      (a, ys) -> pure (a:ys)
    where f k (a:as)
            | k == i = pure (a, x:as)
            | otherwise = do
                r <- f (k+1) as
                case r of
                  (v, vs) -> pure (v, a:vs)
          f _ [] = fail "swapHead: invalid input, list is shorter than the index to swap head of the stack with"

data FakeW64 = FakeW64 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
  deriving Show

toFakeW64 :: Word64 -> FakeW64
toFakeW64 w = FakeW64 w_hi w_lo
  where w_hi = fromIntegral $ shiftR (w .&. 0xFFFFFFFF00000000) 32
        w_lo = fromIntegral $         w .&. 0x00000000FFFFFFFF
fromFakeW64 :: FakeW64 -> Word64
fromFakeW64 (FakeW64 hi lo) = shiftL (fromIntegral hi) 32 .|. fromIntegral lo
