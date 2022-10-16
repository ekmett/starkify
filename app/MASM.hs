{-# LANGUAGE OverloadedStrings #-}
module MASM where

import Data.Text.Lazy (Text, unpack)
import Data.Word

type ProcName = Text

type ModName = Text

data Module = Module
  { moduleImports :: [ModName]
  , moduleProcs :: [Proc]
  , moduleProg  :: Program
  }

data Proc = Proc
  { procName    :: ProcName
  , procNLocals :: Int
  , procInstrs  :: [Instruction]
  }

newtype Program = Program { programInstrs :: [Instruction] }

-- TODO: support whole-word and 8 bits variant of operations that support both.
-- TODO: float "emulation"? ratios, fixed precision, continued fractions, any other relevant construction...

-- TODO(Matthias): perhaps annotate stack effect?
data Instruction
  = Exec ProcName -- exec.foo
  | Push Word32   -- push.n
  | LocStore Word32  -- loc_store.i
  | LocLoad Word32 -- loc_load.i
  | Swap Word32 -- swap[.i]
  | Drop -- drop
  | TruncateStack -- exec.sys::truncate_stack
  | IAdd -- u32checked_add
  | IMul -- u32checked_mul
  | ShL | ShR -- u32checked_{shl, shr}
  | And | Or | Xor -- u32checked_{and, or, xor}
  | EqConst Word32 | Eq | Neq -- u32checked_{eq.n, eq, neq}
  | Lt | Gt -- u32checked_{lt, gt}
  deriving Show

ppMASM :: Module -> String
ppMASM = unlines . ppModule

  where ppModule m = map (("use."++) . unpack) (moduleImports m)
                  ++ concatMap ppProc (moduleProcs m)
                  ++ ppProgram (moduleProg m)

        ppProc p = [ "proc." ++ unpack (procName p) ++ "." ++ show (procNLocals p) ]
                ++ concatMap (map ("  "++) . ppInstr) (procInstrs p)
                ++ [ "end" ]
        ppProgram p = [ "begin" ]
                   ++ concatMap (map ("  "++) . ppInstr) (programInstrs p)
                   ++ [ "end" ]
        ppInstr (Exec pname) = [ "exec." ++ unpack pname ]
        ppInstr (Push n) = [ "push." ++ show n ]
        ppInstr (LocStore n) = [ "loc_store." ++ show n ]
        ppInstr (LocLoad n) = [ "loc_load." ++ show n ]
        ppInstr (Swap n) = [ "swap" ++ if n == 1 then "" else "." ++ show n ]
        ppInstr Drop = [ "drop" ]
        ppInstr TruncateStack = [ "exec.sys::truncate_stack" ]
        ppInstr IAdd = [ "u32checked_add"  ]
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
