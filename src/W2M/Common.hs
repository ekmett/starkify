{-# LANGUAGE OverloadedStrings #-}

module W2M.Common where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W

type WasmAddr = Natural

type MasmAddr = Word32

type LocalAddrs = Map WasmAddr (W.ValueType, [MasmAddr])

type FunName = Text

type PrimFun = FunName

data ModuleInfo = ModuleInfo
  { types :: Vector W.FuncType
  , funcTypes :: Vector W.FuncType
  , exports :: [W.Export]
  , globals :: [W.Global]
  , memBeginning :: MasmAddr
  , globalsAddrMap :: Vector MasmAddr
  , wasiGlobalsAddrMap :: Map Text MasmAddr
  }
  deriving (Eq, Show)

-- Primitive functions

-- | The special name reserved for the 'starkify_call_indirect' procedure
starkifyCallIndirectName :: Text
starkifyCallIndirectName = "starkify_call_indirect"
