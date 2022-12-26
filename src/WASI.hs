{-# LANGUAGE OverloadedStrings #-}

module WASI where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.Lazy (Text)

import MASM qualified as M

data Instruction = M M.Instruction -- MASM instruction
                 | Load Text -- Read field from reserved memory address to the stack
                 | Store Text -- Write field from the stack to reserved memory address

-- WASI methods may require persistent memory and initialization.
data Method = Method
  { globals :: [Text]
  , init :: [Instruction]
  , locals :: Int
  , body :: [Instruction]
  }

library :: Map Text (Map M.ProcName Method)
library = Map.fromList
  [ ("wasi_snapshot_preview1", Map.fromList
     [ ("fd_read", Method
         { globals = ["stdinSize"]
         , init =
             [ M (M.AdvPush 1)
             , Store "stdinSize"
             ]
         , locals = 0
         , body =
             -- fd_read(fd: fd, iovs: iovec_array) -> Result<size, errno>
             -- (func (param i32 i32 i32 i32) (result i32))
             [ M (M.MoveUp 4)
             -- If the file descriptor is any other than 0 (stdin), fail.
             , M M.AssertZ
             , Load "stdinSize"
             -- TODO: Read from the advice tape and store into the vectors.
             , M M.Drop
             , M M.Drop
             , M M.Drop
             , M (M.Push 0)
             ]
         })
     ]) ]