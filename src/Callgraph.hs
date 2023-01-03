module Callgraph where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import GHC.Natural (Natural)
import Language.Wasm.Structure
    ( Function(body),
      Instruction(false, body, true),
      ElemSegment(funcIndexes) )
import Language.Wasm.Structure qualified as W
import W2M.Common
import Prelude hiding (putStrLn, unwords, words)

findCalls :: Instruction Natural -> [GraphFun]
findCalls = \case
  W.Block {body} -> findCalls =<< body
  W.Loop {body} -> findCalls =<< body
  W.If {true, false} -> findCalls =<< true <> false
  W.Call called -> pure $ Right $ fromIntegral called
  W.CallIndirect {} -> pure $ Left starkifyCallIndirectName
  _ -> mempty

type GraphFun = Either PrimFun Int

-- TODO: take code in data segment's offset and elem segment's offset etc into account?
callGraph ::
  [W.ElemSegment] ->
  V.Vector W2M.Common.Function ->
  Map
    GraphFun
    (Set GraphFun)
callGraph elems allFunctions =
  Set.fromList <$> Map.fromListWith (<>) (fromFuns <> fromElems)
  where
    fromFuns, fromElems :: [(GraphFun, [GraphFun])]
    fromFuns =
      V.toList $
        V.indexed allFunctions <&> bimap Right \case
          DefinedFun (W.Function {body}) -> findCalls =<< body
          _ -> []
    fromElems =
      [ ( Left starkifyCallIndirectName,
          [ Right (fromIntegral f)
            | W.ElemSegment {funcIndexes} <- elems,
              f <- funcIndexes
          ]
        )
      ]
