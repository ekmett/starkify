{-# LANGUAGE OverloadedLists #-}
module Tools (dfs) where

import Data.DList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Monad.RWS.Strict
import Data.Foldable (traverse_)

type Graph v = Map v (Set v)

dfs :: Ord v => v -> Map v (Set v) -> [v]
dfs start graph = toList . snd $ execRWS (dfsM start) graph []

dfsM :: Ord v => v -> RWS (Graph v) (DList v) (Set v) ()
dfsM v = do
  visited <- get
  unless (v `Set.member` visited) $ do
    tell [v]
    modify (Set.insert v)
    traverse_ dfsM . Map.findWithDefault [] v =<< ask
