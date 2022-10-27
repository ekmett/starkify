{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module MASM where

import Control.Monad.Writer.Strict

import qualified Data.DList as DList
import Data.Foldable
import Data.Text.Lazy (Text, unpack)
import Data.Typeable
import Data.Word
import Data.String
import qualified GHC.Exts
import GHC.Generics

type ProcName = Text

type ModName = Text

data Module = Module
  { moduleImports :: [ModName]
  , moduleProcs :: [Proc]
  , moduleProg  :: Program
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Proc = Proc
  { procName    :: ProcName
  , procNLocals :: Int
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
  -- https://maticnetwork.github.io/miden/user_docs/assembly/flow_control.html#conditional-execution
  -- Not sure if there's also an if.false?
  -- if.true
  | IfTrue { thenBranch:: [Instruction], elseBranch:: [Instruction] }

  | Push Word32   -- push.n
  | Swap Word32 -- swap[.i]
  | Drop -- drop

  | LocStore Word32  -- loc_store.i
  | LocLoad Word32 -- loc_load.i
  | MemLoad Word32 -- mem_load.a
  -- | MemLoadStack -- mem_load
  | MemStore Word32 -- mem_store.a
  -- | MemStoreStack -- mem_store

  | TruncateStack -- exec.sys::truncate_stack
  | IAdd -- u32checked_add
  | ISub -- "u32checked_sub"
  | IMul -- u32checked_mul
  | ShL | ShR -- u32checked_{shl, shr}
  | And | Or | Xor -- u32checked_{and, or, xor}
  | EqConst Word32 | Eq | Neq -- u32checked_{eq.n, eq, neq}
  | Lt | Gt -- u32checked_{lt, gt}
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype PpMASM a = PpMASM {runPpMASM :: Writer (DList.DList String) a}
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadWriter (DList.DList String) PpMASM

instance (a~()) => IsString (PpMASM a) where
  fromString s = tell [s]

instance (a~()) => GHC.Exts.IsList (PpMASM a) where
  type Item (PpMASM a) = String
  fromList = tell . DList.fromList


indent :: PpMASM a -> PpMASM a
indent = censor (fmap ("  "++))

ppMASM :: Module -> String
ppMASM = unlines . toList . execWriter . runPpMASM . ppModule
  where ppModule m = do
          tell $ DList.fromList $ fmap (("use."++) . unpack) (moduleImports m)
          traverse_ ppProc (moduleProcs m)
          ppProgram (moduleProg m)
        ppProc p = do
          [ "proc." ++ unpack (procName p) ++ "." ++ show (procNLocals p) ]
          indent $ traverse_ ppInstr (procInstrs p)
          "end"
        ppProgram p = do
          "begin"
          indent $ traverse_ ppInstr (programInstrs p)
          "end"
        ppInstr :: Instruction -> PpMASM ()
        ppInstr (Exec pname) = [ "exec." ++ unpack pname ]
        ppInstr (IfTrue thenBranch elseBranch) = do
          "if.true"
          indent $ traverse_ ppInstr thenBranch
          "else"
          indent $ traverse_ ppInstr elseBranch
          "end"

        ppInstr (LocStore n) = [ "loc_store." ++ show n ]
        ppInstr (LocLoad n) = [ "loc_load." ++ show n ]
        ppInstr (MemStore n) = [ "mem_store." ++ show n ]
        ppInstr (MemLoad n) = [ "mem_load." ++ show n ]

        ppInstr (Push n) = [ "push." ++ show n ]
        ppInstr (Swap n) = [ "swap" ++ if n == 1 then "" else "." ++ show n ]
        ppInstr Drop = [ "drop" ]
        ppInstr TruncateStack = [ "exec.sys::truncate_stack" ]

        ppInstr IAdd = [ "u32checked_add"  ]
        ppInstr ISub = [ "u32checked_sub"  ]
        ppInstr IMul = [ "u32checked_mul" ]
        ppInstr ShL = [ "u32checked_shl" ]
        ppInstr ShR = [ "u32checked_shr" ]
        ppInstr (EqConst k) = [ "u32checked_eq." ++ show k ]
        ppInstr Eq = [ "u32checked_eq" ]
        ppInstr Neq = [ "u32checked_neq" ]
        ppInstr Lt = [ "u32checked_lt" ]
        ppInstr Gt = [ "u32checked_gt" ]
        ppInstr And = [ "u32checked_and" ]
        ppInstr Or = [ "u32checked_or" ]
        ppInstr Xor = [ "u32checked_xor" ]
mod1 :: Module
mod1 = Module
  { moduleImports = [ "std::sys" ]
  , moduleProcs = [proc1, proc2]
  , moduleProg = Program [ Exec "hello" ]
  }
  where proc1 = Proc "hello" 3 []
        proc2 = Proc "world" 2 []
