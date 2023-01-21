module MASM.Callgraph where

import Control.Monad
import Control.Applicative
import Data.Graph qualified as Graph
import Prelude hiding (putStrLn, unwords, words)
import MASM.Types qualified as M

findCalls :: M.Proc -> [M.ProcName]
findCalls M.Proc {procInstrs} = findCalls' =<< procInstrs

findCalls' :: M.Instruction -> [M.ProcName]
findCalls' (M.Exec name) = [name]
findCalls' M.If {thenBranch, elseBranch} =
  findCalls' =<< thenBranch <> elseBranch
findCalls' (M.While body) = findCalls' =<< body
findCalls' _ = []

-- Miden requires procedures to be defined before any execs that reference them.
sortProcs :: [(M.ProcName, M.Proc)] -> [(M.ProcName, M.Proc)]
sortProcs procs = extract . v2node <$> Graph.reverseTopSort callGraph
  where
    (callGraph, v2node, _) = Graph.graphFromEdges $ inject <$> procs
    inject (name, proc) = (proc, name, findCalls proc)
    extract (proc, name, _) = (name, proc)
