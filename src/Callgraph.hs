module Callgraph where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Graph qualified as Graph
import Data.Maybe ( mapMaybe )
import Data.Tuple.Select (sel2)
import Data.Vector qualified as V
import GHC.Natural (Natural)
import Language.Wasm.Structure
  ( ElemSegment (funcIndexes),
    Instruction (body, false, true),
  )
import Language.Wasm.Structure qualified as W
import W2M.Common
import Prelude hiding (putStrLn, unwords, words)

type GraphFun = Either PrimFun Int

findCalls :: Instruction Natural -> [GraphFun]
findCalls = \case
  W.Block {body} -> findCalls =<< body
  W.Loop {body} -> findCalls =<< body
  W.If {true, false} -> findCalls =<< true <> false
  W.Call called -> pure $ Right $ fromIntegral called
  W.CallIndirect {} -> pure $ Left starkifyCallIndirectName
  _ -> mempty

indirectCalls :: [ElemSegment] -> [(GraphFun, [GraphFun])]
indirectCalls elems =
  [ ( Left starkifyCallIndirectName,
      [ Right $ fromIntegral f
        | W.ElemSegment {funcIndexes} <- elems,
          f <- funcIndexes
      ]
    )
  ]

directCalls :: V.Vector Function -> [(GraphFun, [GraphFun])]
directCalls allFunctions =
  [ (Right caller, findCalls =<< body)
    | (caller, DefinedFun (W.Function {body})) <- V.toList $ V.indexed allFunctions
  ]

-- TODO: take code in data segment's offset and elem segment's offset etc into account?
allCalls :: V.Vector Function -> [ElemSegment] -> [((), GraphFun, [GraphFun])]
allCalls allFunctions elems =
  directCalls allFunctions ++ indirectCalls elems <&> \(s, t) ->
    ((), s, t)

getSortedFunctions ::
  V.Vector Function ->
  [GraphFun] ->
  [ElemSegment] ->
  [GraphFun]
getSortedFunctions allFunctions entryFunctions elems =
  fmap (sel2 . v2node)
    . nubOrd
    . reverse
    . Graph.reachable callGraph
    =<< mapMaybe k2v entryFunctions
  where
    (callGraph, v2node, k2v) = Graph.graphFromEdges $ allCalls allFunctions elems
