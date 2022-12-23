{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module MASM where

import Control.Monad.Writer.Strict

import Data.DList qualified as DList
import Data.Text.Lazy qualified as T
import Data.Foldable
import Data.Text.Lazy (Text, unpack)
import Data.Typeable
import Data.Word
import Data.String
import GHC.Exts qualified
import GHC.Generics

type ProcName = Text

type ModName = Text

data Module = Module
  { moduleImports :: [ModName]
  , moduleProcs :: [(ProcName, Proc)]
  , moduleProg  :: Program
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Proc = Proc
  { procNLocals :: Int
  , procInstrs  :: [Instruction]
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype Program = Program { programInstrs :: [Instruction] }
  deriving (Eq, Ord, Show, Generic, Typeable)

-- TODO: support whole-word and 8 bits variant of operations that support both.
-- TODO: float "emulation"? ratios, fixed precision, continued fractions, any other relevant construction...

-- TODO(Matthias): perhaps annotate stack effect?
data Instruction
  = Exec ProcName -- exec.foo
  | If { -- if.true
    thenBranch :: [Instruction],
    elseBranch :: [Instruction] }
  | While [Instruction] -- while.true

  | AdvPush Word32 -- adv_push.n
  | Push Word32   -- push.n
  | Swap Word32 -- swap[.i]
  | Drop -- drop
  | CDrop -- cdrop
  | Dup Word32 -- dup.n
  | MoveUp Word32 -- movup.n
  | TruncateStack -- exec.sys::truncate_stack
  | SDepth -- sdepth
  | Eq (Maybe Word32) -- eq[.n]
  | NEq (Maybe Word32) -- neq[.n]
  | Not -- not

  | LocStore Word32  -- loc_store.i
  | LocLoad Word32 -- loc_load.i
  | MemLoad (Maybe Word32) -- mem_load[.i]
  | MemStore (Maybe Word32) -- mem_store[.i]
  | Add (Maybe Word32) -- add[.n]
  | Sub (Maybe Word32) -- sub[.n]

  | IAdd -- u32checked_add
  | ISub -- "u32checked_sub"
  | IMul -- u32checked_mul
  | IDiv -- u32checked_div
  | IMod -- u32checked_mod
  | IDivMod (Maybe Word32) -- u32checked_divmod
  | IShL | IShR -- u32checked_{shl, shr}
  | IAnd | IOr | IXor | INot -- u32checked_{and, or, xor, not}
  | IEq (Maybe Word32) | INeq -- u32checked_{eq[.n], neq}
  | ILt | IGt | ILte | IGte -- u32checked_{lt[e], gt[e]}

  -- "faked 64 bits" operations, u64::checked_{add,sub,mul}
  | IAdd64 | ISub64 | IMul64
  | IShL64 | IShR64
  | IOr64 | IAnd64 | IXor64
  | IEq64 | IEqz64 | INeq64
  | ILt64 | IGt64 | ILte64 | IGte64

  | Assert
  | AssertZ
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype PpMASM a = PpMASM {runPpMASM :: Writer (DList.DList String) a}
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadWriter (DList.DList String) PpMASM

instance (a~()) => IsString (PpMASM a) where
  fromString s = tell [s]

instance (a~()) => GHC.Exts.IsList (PpMASM a) where
  type Item (PpMASM a) = String
  fromList = tell . DList.fromList
  toList = DList.toList . snd . runWriter . runPpMASM

accessibleStackDepth :: Int
accessibleStackDepth = 16

indent :: PpMASM a -> PpMASM a
indent = censor (fmap ("  "++))

ppMASM :: Module -> String
ppMASM = unlines . toList . execWriter . runPpMASM . ppModule
  where ppModule m = do
          tell $ DList.fromList $ fmap (("use."++) . unpack) (moduleImports m)
          traverse_ ppProc (moduleProcs m)
          ppProgram (moduleProg m)
        ppProc (name, p) = do
          [ "proc." ++ T.unpack name ++ "." ++ show (procNLocals p) ]
          indent $ traverse_ ppInstr (procInstrs p)
          "end"
        ppProgram p = do
          "begin"
          indent $ traverse_ ppInstr (programInstrs p)
          "end"
        ppInstr :: Instruction -> PpMASM ()
        ppInstr (Exec pname) = [ "exec." ++ unpack pname ]
        ppInstr (If {thenBranch, elseBranch}) = do
          "if.true"
          indent $ traverse_ ppInstr thenBranch
          unless (null elseBranch) $ do
            "else"
            indent $ traverse_ ppInstr elseBranch
          "end"
        ppInstr (While body) = do
          "while.true"
          indent $ traverse_ ppInstr body
          "end"

        ppInstr (LocStore n) = [ "loc_store." ++ show n ]
        ppInstr (LocLoad n) = [ "loc_load." ++ show n ]

        ppInstr (AdvPush n) = [ "adv_push." ++ show n ]
        ppInstr (Push n) = [ "push." ++ show n ]
        ppInstr (Swap n) = [ "swap" ++ if n == 1 then "" else "." ++ show n ]
        ppInstr Drop = "drop"
        ppInstr CDrop = "cdrop"
        ppInstr (Dup n) = [ "dup." ++ show n ]
        ppInstr (MoveUp n) = [ "movup." ++ show n ]
        ppInstr TruncateStack = "exec.sys::truncate_stack"
        ppInstr SDepth = "sdepth"
        ppInstr (Eq Nothing) = "eq"
        ppInstr (Eq (Just n)) = [ "eq." ++ show n ]
        ppInstr (NEq Nothing) = "neq"
        ppInstr (NEq (Just n)) = [ "neq." ++ show n ]
        ppInstr Not = "not"

        ppInstr (Add Nothing) = "sub"
        ppInstr (Add (Just n)) = [ "add." ++ show n ]
        ppInstr (Sub Nothing) = "sub"
        ppInstr (Sub (Just n)) = [ "sub." ++ show n ]

        ppInstr IAdd = "u32wrapping_add"
        ppInstr ISub = "u32wrapping_sub"
        ppInstr IMul = "u32wrapping_mul"
        ppInstr IDiv = "u32checked_div"
        ppInstr IMod = "u32checked_mod"
        ppInstr (IDivMod mk) = [ "u32checked_divmod" ++ maybe "" (\k -> "." ++ show k) mk ]
        ppInstr IShL = "u32checked_shl"
        ppInstr IShR = "u32checked_shr"
        ppInstr (IEq mk) = [ "u32checked_eq" ++ maybe "" (\k -> "." ++ show k) mk ]
        ppInstr INeq = "u32checked_neq"
        ppInstr ILt = "u32checked_lt"
        ppInstr IGt = "u32checked_gt"
        ppInstr ILte = "u32checked_lte"
        ppInstr IGte = "u32checked_gte"
        ppInstr IAnd = "u32checked_and"
        ppInstr IOr = "u32checked_or"
        ppInstr IXor = "u32checked_xor"
        ppInstr INot = "u32checked_not"

        ppInstr (MemLoad mi) = [ "mem_load" ++ maybe "" (\i -> "." ++ show i) mi ]
        ppInstr (MemStore mi) = [ "mem_store" ++ maybe "" (\i -> "." ++ show i) mi ]
        ppInstr IAdd64 = "exec.u64::wrapping_add"
        ppInstr ISub64 = "exec.u64::checked_sub"
        ppInstr IMul64 = "exec.u64::checked_mul"
        ppInstr IEq64 = "exec.u64::checked_eq"
        ppInstr INeq64 = "exec.u64::checked_neq"
        ppInstr IEqz64 = "exec.u64::checked_eqz"
        ppInstr ILt64 = "exec.u64::checked_lt"
        ppInstr IGt64 = "exec.u64::checked_gt"
        ppInstr ILte64 = "exec.u64::checkted_lte"
        ppInstr IGte64 = "exec.u64::checked_gte"
        ppInstr IShL64 = "exec.u64::overflowing_shl"
        ppInstr IShR64 = "exec.u64::overflowing_shr"
        ppInstr IOr64 = "exec.u64::checked_or"
        ppInstr IAnd64 = "exec.u64::checked_and"
        ppInstr IXor64 = "exec.u64::checked_xor"

        ppInstr Assert = "assert"
        ppInstr AssertZ = "assertz"