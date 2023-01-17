{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Continuations where

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W
import Prelude hiding (putStrLn, unwords, words)

data Exit
  = Local
  | Br Natural
  | Return
  deriving (Eq, Ord, Show)

newtype Exits = Exits {getExits :: Set Exit}
  deriving (Eq, Ord, Show)

instance Semigroup Exits where
  -- Essentially this replaces Local in a
  -- with all of b.  There's probably a way
  -- to use Control.Lens.At for this. ;)
  Exits a <> Exits b
    | Set.notMember Local a = Exits a
    | otherwise =
        Exits $ Set.delete Local a <> b

instance Monoid Exits where
  mempty = Exits [Local]

mergeExits :: Exits -> Exits -> Exits
mergeExits (Exits a) (Exits b) = Exits (Set.union a b)

-- At end of block and if and function:
endBlock :: Exits -> Exits
endBlock = Exits . Set.map down1 . getExits

down1 :: Exit -> Exit
down1 Local = Local
down1 (Br 0) = Local
down1 (Br x) = Br (x - 1)
down1 Return = Return

endLoop :: Exits -> Exits
-- Br 0 means just going back to start of loop.
endLoop = Exits . Set.delete (Br 0) . getExits

findExits :: W.Instruction Natural -> Set Exit
findExits = getExits . findExits'

findExits' :: W.Instruction Natural -> Exits
findExits' = \case
  W.Block {body} ->
    endBlock $ foldMap findExits' body
  W.Loop {body} ->
    endBlock $ endLoop $ foldMap findExits' body
  W.If {true, false} ->
    endBlock $ foldMap findExits' true `mergeExits` foldMap findExits' false
  W.BrTable cases defaultIdx ->
    Exits . Set.fromList . fmap Br $ defaultIdx : cases
  W.Br index -> Exits [Br index]
  W.BrIf index -> Exits [Br index, Local]
  W.Return -> Exits [Return]
  W.Unreachable -> Exits []
  _ -> Exits [Local]
