{-# LANGUAGE OverloadedStrings #-}

module W2M.Common where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Word (Word32)
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W

type WasmAddr = Natural

type MasmAddr = Word32

type LocalAddrs = Map WasmAddr (W.ValueType, [MasmAddr])

type FunName = Text

type PrimFun = FunName

data Function = ImportedFun W.Import | StarkifyFun FunName | DefinedFun W.Function

-- Primitive functions

-- | The special name reserved for the 'starkify_call_indirect' procedure
starkifyCallIndirectName :: Text
starkifyCallIndirectName = "starkify_call_indirect"

-- -- | We simulate that the function is part of the WASM module, with index
-- --   @largest function index in the module + 1@.
-- starkifyCallIndirectId :: Integral a => Vector Function -> a
-- starkifyCallIndirectId funs = fromIntegral (V.length funs)

primitiveFuns :: [Function]
primitiveFuns = [StarkifyFun starkifyCallIndirectName]
