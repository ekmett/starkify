{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module W2M where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.ByteString.Lazy qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Functor ((<&>), ($>))
import Data.List (stripPrefix, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8, Word32)
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W

import Continuations qualified as C
import MASM qualified as M
import MASM.Types qualified as M
import MASM.Interpreter (toFakeW64, FakeW64 (..))
import Validation
import WASI qualified
import GHC.Stack (HasCallStack)

import W2M.Common

-- Note: Wasm modules may fail to compile if they contain > 2^29 functions.

-- Reserved masm addresses:
branchCounter :: MasmAddr
branchCounter = 0

{-

The memory at the branchCounter address (henceforth just branchCounter
for simplicity) is used when we are simulating a non-local exit from
WASM instructions like `br` or `return`.

Crucially, we translate `br index` to a branchCounter of `index + 1`.
That is so that we have a special value, `0` to indicate that we aren't
doing a non-local exit at the moment.

In most situations, `br 0` and a local exit are indistinguishable at the end
of a block.  The only reason we need to distinguish between the two are br
that target a `loop`.

For a loop a normal local exit just exits the loop, and a br to the loop
starts one more iteration.

-}

firstNonReservedAddress :: MasmAddr
firstNonReservedAddress = branchCounter + 1

numCells :: Num a => W.ValueType -> a
numCells W.I32 = 1
numCells W.I64 = 2
numCells t = error $ "unsupported value type: " ++ show t

ncells :: W.Global -> MasmAddr
ncells = numCells . globalTypeToValue . W.globalType

globalTypeToValue :: W.GlobalType -> W.ValueType
globalTypeToValue (W.Const vt) = vt
globalTypeToValue (W.Mut vt) = vt

getExports :: V [W.Export]
getExports = exports <$> getModuleInfo

getTypes :: V (Vector W.FuncType)
getTypes = types <$> getModuleInfo

getType :: Integral ti => ti -> V W.FuncType
getType ti = do
  types <- getTypes
  pure $ types ! fromIntegral ti

getFuncTypes :: V (Vector W.FuncType)
getFuncTypes = funcTypes <$> getModuleInfo

getMemBeginning :: V MasmAddr
getMemBeginning = memBeginning <$> getModuleInfo

getGlobals :: V [W.Global]
getGlobals = globals <$> getModuleInfo

getGlobalsAddrMap :: V (Vector MasmAddr)
getGlobalsAddrMap = globalsAddrMap <$> getModuleInfo

getGlobalAddr :: Integral k => k -> V MasmAddr
getGlobalAddr k = do
  globalsAddrMap <- getGlobalsAddrMap
  pure $ globalsAddrMap ! fromIntegral k

wasiImport :: W.Import -> Maybe WASI.Method
wasiImport (W.Import module' name _) = Map.lookup name =<< Map.lookup module' WASI.library

makeModuleInfo :: W.Module -> ModuleInfo
makeModuleInfo m =  ModuleInfo
      { types, funcTypes
      , exports = W.exports m, globals = W.globals m
      , memBeginning, globalsAddrMap, wasiGlobalsAddrMap }
  where
    types :: Vector W.FuncType
    types = V.fromList $ W.types m
    funcTypes :: Vector W.FuncType
    funcTypes = V.fromList . fmap ((types !) . fromIntegral) $
        importedFunsTypeIndex <> definedFunTypeIdxs

    importedFunsTypeIndex :: [Natural]
    importedFunsTypeIndex = [ idx | W.Import _ _ (W.ImportFunc idx) <- W.imports m ]
    definedFunTypeIdxs :: [Natural]
    definedFunTypeIdxs = W.funcType <$> W.functions m

    memBeginning' :: MasmAddr
    memBeginning' = maximum (firstNonReservedAddress : Map.elems wasiGlobalsAddrMap)

    globalsAddrMap' :: [MasmAddr]
    globalsAddrMap' = scanl (+) memBeginning' $ fmap ncells (W.globals m)

    globalsAddrMap :: Vector MasmAddr
    globalsAddrMap = V.fromList (init globalsAddrMap')

    memBeginning :: MasmAddr
    memBeginning = last globalsAddrMap'

    wasiGlobals :: [Text]
    wasiGlobals = WASI.globals <=< mapMaybe wasiImport $ W.imports m

    wasiGlobalsAddrMap :: Map Text MasmAddr
    wasiGlobalsAddrMap = Map.fromList (zip wasiGlobals [firstNonReservedAddress..])

translateGlobals :: WASI.Instruction -> V [M.Instruction]
translateGlobals = \case
  WASI.M i -> pure [i]
  WASI.Load n -> f n M.MemLoad
  WASI.Store n -> f n M.MemStore
  where
    f n memOp =
      Map.findWithDefault (badNamedGlobalRef n) n
      . fmap (\a -> pure [memOp (Just a)])
      . wasiGlobalsAddrMap =<< getModuleInfo

getDatasInit :: [W.DataSegment] -> V [M.Instruction]
getDatasInit datas = inContext DatasInit $
  concat <$> traverse getDataInit datas

getDataInit :: W.DataSegment -> V [M.Instruction]
getDataInit (W.DataSegment 0 offset_wexpr bytes) = do
  memBeginning <- getMemBeginning
  offset_mexpr <- translateInstrs offset_wexpr
  pure $ offset_mexpr ++
          [ M.Push 4, M.IDiv             -- [offset_bytes/4, ...]
          , M.Push memBeginning, M.IAdd  -- [offset_bytes/4+memBeginning, ...] =
          ] ++                           -- [addr_u32, ...]
          writeW32s (BS.unpack bytes) ++ -- [addr_u32+len(bytes)/4, ...]
          [ M.Drop ]                     -- [...]
getDataInit _ = badNoMultipleMem

getGlobalsInit :: [W.Global] -> V [M.Instruction]
getGlobalsInit globals = inContext GlobalsInit $
  concat <$> zipWithM getGlobalInit [0..] globals

getGlobalInit :: Int -> W.Global -> V [M.Instruction]
getGlobalInit k g =
  translateInstrs (W.initializer g ++ [W.SetGlobal $ fromIntegral k])

initImport :: W.Import -> V [M.Instruction]
initImport import_
  = fmap (concat . concat)
  . (traverse . traverse) translateGlobals
  . fmap WASI.init
  $ wasiImport import_
initImports :: [W.Import] -> V [M.Instruction]
initImports imports_ = inContext ImportInit $
  join <$> traverse initImport imports_

-- Each compiler has a different convention for exporting the main function, and the
-- https://www.w3.org/TR/wasm-core-1/#start-function is something different. Since we don't
-- currently pass input to the main function, we can proceed if either is present (and we
-- should use both if both are present).
findEntryFunctions :: W.Module -> [M.ProcName]
findEntryFunctions m = procName <$> nubOrd (maybeToList startFunIdx <> maybeToList mainFunIdx)
  where
  -- An export with an empty string is considered to be a "default export".
  -- (https://github.com/bytecodealliance/wasmtime/blob/b0939f66267dc99b56f59fdb7c1db4fce2f578c6/crates/wasmtime/src/linker.rs#L1187)
  mainFunIdx = lookup "main" exportedFunctions
            <|> lookup "_start" exportedFunctions
            <|> lookup "" exportedFunctions

  exportedFunctions :: [(FunName, W.FuncIndex)]
  exportedFunctions = [(name, idx) | (W.Export name (W.ExportFunc idx)) <- W.exports m]

  startFunIdx
    | Just (W.StartFunction k) <- W.start m = Just k
    | otherwise = Nothing

type WithFunCounter a = StateT Int V a
withFunCounter :: (Int -> V M.Proc) -> WithFunCounter (M.ProcName, M.Proc)
withFunCounter makeProc = do
  funId <- get
  modify' (1+)
  proc <- lift $ makeProc funId
  return (procName funId, proc)

toMASM :: W.Module -> V M.Module
toMASM m = inContext InModule { moduleInfo = makeModuleInfo m } do
  globalsInit <- getGlobalsInit (W.globals m)
  datasInit <- getDatasInit (W.datas m)
  -- TODO: don't throw away main's type, we might want to check it and inform how the program can be called?
  let entryFunctions = findEntryFunctions m
  when (null entryFunctions) badNoMain

  methodInits <- initImports (W.imports m)
  procs <- allProcs

  pure $ M.Module ["std::sys", "std::math::u64"]
    procs
    -- TODO: Do we need to perform stack cleanup even if proc_exit is invoked?
    (M.Program $
      globalsInit
      ++ datasInit
      ++ methodInits
      ++ fmap M.Exec entryFunctions)

  where
    importedFuns :: [W.Import]
    importedFuns = [ f | f@(W.Import _ _ (W.ImportFunc _)) <- W.imports m ]

    definedFuns :: [W.Function]
    definedFuns = W.functions m

-- "Functions are referenced through function indices,
--  starting with the smallest index not referencing a function import."
-- (https://webassembly.github.io/spec/core/syntax/modules.html#syntax-module)
-- "Definitions are referenced with zero-based indices."
-- (https://webassembly.github.io/spec/core/syntax/modules.html#syntax-funcidx)
    allProcs :: V (Map M.ProcName M.Proc)
    allProcs = do
      d <- (starkifyCallIndirectName,) <$> mkStarkifyCallIndirect (W.elems m)
      procs <- evalStateT `flip` 0 $ do
        importProcs <- traverse withFunCounter
          (const . import2MASM <$> importedFuns)
        defProcs <- traverse withFunCounter
          (fun2MASM <$> definedFuns)
        pure $ importProcs <> defProcs
      return . Map.fromList $ d : procs

fun2MASM :: W.Function -> Int -> V M.Proc
fun2MASM (W.Function typ wasm_locals body) funcId = do
      wasm_args <- W.params <$> getType typ
      let localAddrs :: LocalAddrs
          (nlocalCells, localAddrs) =
            let argsAndLocals = (wasm_args ++ wasm_locals)
                sizes = fmap numCells argsAndLocals
                starts = scanl (+) 0 sizes
                addresses = zipWith (\start end -> [start..end-1])
                  starts (drop 1 starts)
              in (last starts, Map.fromList $ zip [0..] (zip argsAndLocals addresses))

          -- the function starts by populating the first nargs local vars
          -- with the topmost nargs values on the stack, removing them from
          -- the stack as it goes. it assumes the value for the first arg
          -- was pushed first, etc, with the value for the last argument
          -- being pushed last and therefore popped first.
          prelude = reverse $ concat
            [ case Map.lookup (fromIntegral k) localAddrs of
                Just (_t, is) -> map M.LocStore is
                _ -> error ("impossible: prelude of procedure " ++ show funcId ++ ", local variable " ++ show k ++ " not found?!")
            | k <- [0..length wasm_args - 1]
            ]
      inContext InFunction {funcId, localAddrs} do
        instrs <- translateInstrs body
        return $ M.Proc (fromIntegral nlocalCells) (prelude ++ instrs)

import2MASM :: W.Import -> V M.Proc
import2MASM import_
  = inContext Import case wasiImport import_ of
    Nothing -> badImport import_
    Just method -> do
      let procNLocals = WASI.locals method
      is <- concat <$> traverse translateGlobals (WASI.body method)
      pure M.Proc
        { procNLocals
        , procInstrs = M.comment import_ : is }

translateInstrs :: W.Expression -> V [M.Instruction]
translateInstrs = foldr f (pure []) . zip [0..] where
  -- inContext (InInstruction instructionCount i)
  f (instructionCount, i) localExit = do
    localExit' <- fixCurrentContext localExit
    inContext (InInstruction instructionCount i) $
      translateControlInstr i localExit' <|>
      ((<>) <$> cut (translateInstr' i) <*> localExit')
  translateInstr' i = (M.comment i :) <$> translateInstr i

translateControlInstr :: W.Instruction Natural -> V [M.Instruction] -> V [M.Instruction]
translateControlInstr i@(W.Block t body) localExit = do
  stack <- get
  bt <- blockType t
  -- TODO(Matthias):
  -- This whole section reminds me of Twisted Functors.
  -- W.FuncType forms a monoid, and it acts on [W.ValueType]
  -- Refactor?
  let ctx = InBlock Block t stack
  body' <-
      inContext ctx
    $ withLocalStack (W.params bt)
    $ translateInstrs body
  typedF bt
  is' <- continue i localExit
  pure $ M.comment ctx : body' <> is'
translateControlInstr i@(W.Loop t body) localExit = do
  stack <- get
  bt <- blockType t
  let ctx = InBlock Loop t stack
  body' <-
      inContext ctx
    $ withLocalStack (W.params bt)
    $ translateInstrs body
  typedF bt
  is' <- continue i localExit
  pure $ [M.comment ctx, M.Push 1, M.While (body' <> continueLoop)] <> is'
translateControlInstr i@(W.If t tb fb) localExit = do
  bt <- blockType t
  stack <- get
  let ctx = InBlock If t stack
      makeContext = withLocalStack (W.params bt)
                  . inContext ctx
  body <-
      M.If <$> makeContext (translateInstrs tb)
           <*> makeContext (translateInstrs fb)
  typedF bt
  is' <- continue i localExit
  let body' = [M.NEq (Just 0), body]
  pure $ M.comment ctx : body' <> is'

translateControlInstr i@(W.Br idx) _ = (M.comment i:) <$> branch idx
translateControlInstr i@(W.BrIf idx) localExit = do
  typed [W.I32] []
  br <- branch idx
  is' <- localExit
  pure [M.comment i, M.NEq (Just 0), M.If br is']
-- Note: br_table could save 2 cycles by not duping and dropping in the final case (for br_tables with 1 or more cases).
translateControlInstr i@(W.BrTable cases defaultIdx) _ = do
  typed [W.I32] []
  let branch' = fmap (M.Drop :) . branch
      -- Step through our table,
      -- and reduce the working index as we go along.
      -- Stop and branch when either the index is 0
      -- or we run out of table.
      step br rest =
        [ M.Dup 0, M.Eq (Just 0)
        , M.If br (M.Sub (Just 1) : rest)]
  (M.comment i :) . foldr1 step <$> mapM branch' (cases ++ [defaultIdx])
translateControlInstr i@W.Return _ =
  fmap (M.comment i :) . branch . fromIntegral =<< blockDepth
translateControlInstr i@W.Unreachable _ = pure
  [M.comment i, M.Push 0, M.Assert]
translateControlInstr i@(W.Call idx) localExit = do
  W.FuncType params res <- getFunctionType idx
  params' <- checkTypes params
  res' <- checkTypes res
  let name = procName (fromIntegral idx :: Integer)
  (<>) <$>
    (typed (reverse params') res' $> [M.comment i, M.Exec name])
    <*> localExit

translateControlInstr i@(W.CallIndirect tyIdx) localExit =
  getType tyIdx >>= \(W.FuncType paramsTys retTys) -> do
    params <- checkTypes paramsTys
    ret    <- checkTypes retTys
    liftA2 (<>)
      (typed (W.I32:reverse params) ret $> [M.comment i, M.Exec starkifyCallIndirectName])
      localExit

translateControlInstr i _ = unsupportedInstruction i

exportedName :: Integral i => i -> V (Maybe FunName)
exportedName i = do
  exports <- getExports
  return $ lookup (fromIntegral i) [(idx, name) | (W.Export name (W.ExportFunc idx)) <- exports]

-- TODO: Uniquify names if necessary (import/export conflicts or exported names like "f1").
procName :: Integral i => i -> M.ProcName
procName i = "f" <> T.pack (show (fromIntegral i :: Integer))


getGlobalTy :: Integral k => k -> V W.ValueType
getGlobalTy k = do
  globals <- getGlobals
  pure $ globalTypeToValue . W.globalType $ globals !! fromIntegral k

translateInstr :: W.Instruction Natural -> V [M.Instruction]
translateInstr W.Nop = pure []
translateInstr (W.I32Const w32) = typed [] [W.I32] $> [M.Push w32]
translateInstr (W.IUnOp bitsz op) = translateIUnOp bitsz op
translateInstr (W.IBinOp bitsz op) = translateIBinOp bitsz op
translateInstr W.I32Eqz = typed [W.I32] [W.I32] $> [M.IEq (Just 0)]
translateInstr (W.IRelOp bitsz op) = translateIRelOp bitsz op
translateInstr W.Select = cut $ asum
  [ typed [W.I32, W.I32, W.I32] [W.I32] $> oneCell
  , typed [W.I32, W.F32, W.F32] [W.F32] $> oneCell
  , typed [W.I32, W.I64, W.I64] [W.I64] $> twoCells
  ]
  where
    oneCell = [M.Eq (Just 0), M.CDrop]
    twoCells =
      [ M.NEq (Just 0)       -- [c, f1, f2, t1, t2, ...]
      , M.If                 -- [f1, f2, t1, t2, ...]
        [ M.Drop, M.Drop ]   -- [t1, t2, ...]
        [ M.MoveUp 2, M.Drop -- [f1, f2, t2, ...]
        , M.MoveUp 2, M.Drop -- [f1, f2, ...]
        ]
      ]
translateInstr (W.I32Load (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  typed [W.I32] [W.I32] $>
    -- assumes byte_addr is divisible by 4 and ignores remainder... hopefully it's always 0?
    [ M.Push 4
    , M.IDiv
    , M.Push (fromIntegral offset `div` 4)
    , M.IAdd
    , M.Push memBeginning
    , M.IAdd
    , M.MemLoad Nothing
    ]
translateInstr (W.I32Store (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  -- we need to turn [val, byte_addr, ...] of wasm into [u32_addr, val, ...]
  typed [W.I32, W.I32] [] $>
    -- assumes byte_addr is divisible by 4 and ignores remainder... hopefully it's always 0?
    [ M.Swap 1
    , M.Push 4
    , M.IDiv
    , M.Push (fromIntegral offset `div` 4)
    , M.IAdd
    , M.Push memBeginning
    , M.IAdd
    , M.MemStore Nothing
    ]
translateInstr (W.I32Load8U (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  typed [W.I32] [W.I32] $>
    [ M.Push (fromIntegral offset) -- [offset, byte_addr, ...]
    , M.IAdd                       -- [byte_addr+offset, ...]
    , M.IDivMod (Just 4)           -- [r, q, ...]
                                  -- where byte_addr+offset = 4*q + r
    , M.Swap 1                     -- [q, r, ...]
    , M.Push memBeginning
    , M.IAdd                       -- [memBeginning+q, r, ...]
    , M.MemLoad Nothing            -- [v, r, ...]
    , M.Swap 1                     -- [r, v, ...]
    -- we have an i32 (v), but we need just the 8 bits between spots 8*r and 8*r+7
    -- so we AND with the right mask and shift the result right by 8*r bits.
    -- (v & mask) << (8*r) gives us (as an i32) the value of the 8-bits starting
    -- at position 8*r. e.g (with lowest bits on the left):
    -- v    = xxxxxxxx|abcdefgh|xxxxxxxx|xxxxxxxx
    -- mask = 00000000|11111111|00000000|00000000
    -- and  = 00000000|abcdefgh|00000000|00000000
    -- res  = abcdefgh|00000000|00000000|00000000
    -- note: 11111111 is 255
    , M.Push 8, M.IMul     -- [8*r, v, ...]
    , M.Swap 1, M.Dup 1    -- [8*r, v, 8*r, ...]
    , M.Push 255, M.Swap 1 -- [8*r, 255, v, 8*r...]
    , M.IShL               -- [mask, v, 8*r, ...]
    , M.IAnd               -- [and, 8*r, ...]
    , M.Swap 1, M.IShR     -- [res, ...]
    ]
translateInstr (W.I32Load8S mem) = do
  loadInstrs <- translateInstr (W.I32Load8U mem)
  sigInstrs <- typed [W.I32] [W.I32] $>       -- [v, ...]
            [ M.Dup 0                         -- [v, v, ...]
            , M.Push 128, M.IGte              -- [v >= 128, v, ...]
            , M.If                            -- [v, ...]
                [ M.Push 255, M.Swap 1        -- [v, 255, ...]
                , M.ISub, M.Push 1, M.IAdd    -- [255 - v + 1, ...]
                , M.Push 4294967295           -- [4294967295, 255 - v + 1, ...]
                , M.Swap 1, M.ISub            -- [4294967295 - (255 - v + 1), ...]
                , M.Push 1, M.IAdd            -- [4294967295 - (255 - v + 1) + 1, ...]
                ]
                [] -- if the 32 bits of v encode a positive 8 bits number, nothing to do
            ]
  pure $ loadInstrs <> sigInstrs
translateInstr (W.I32Store8 (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  -- we have an 8-bit value stored in an i32, e.g (lowest on the left):
  -- i   = abcdefgh|00000000|00000000|00000000
  -- there's an i32 value stored at addr q, e.g:
  -- v   = xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx
  -- and we want to update the 8*r to 8*r+7 bits of v with
  -- the first 8 bits of i, so in the example ending with:
  -- res = xxxxxxxx|abcdefgh|xxxxxxxx|xxxxxxxx
  -- we get there by shifting i by 8*r bits to the "left":
  -- i'  = 00000000|abcdefgh|00000000|00000000
  -- setting free the relevant bits in v:
  -- v'  = xxxxxxxx|00000000|xxxxxxxx|xxxxxxxx
  -- and storing v' | i'
  typed [W.I32, W.I32] [] $>
    [ M.Swap 1                     -- [byte_addr, i, ...]
    , M.Push (fromIntegral offset) -- [offset, byte_addr, i, ...]
    , M.IAdd                       -- [byte_addr+offset, i, ...]
    , M.IDivMod (Just 4)           -- [r, q, i, ...]
                                  -- where byte_addr+offset = 4*q + r
    , M.Push 8, M.IMul             -- [8*r, q, i, ...]
    , M.Dup 0                      -- [8*r, 8*r, q, i, ...]
    , M.Push 255, M.Swap 1         -- [8*r, 255, 8*r, q, i, ...]
    , M.IShL, M.INot               -- [mask, 8*r, q, i, ...]
    , M.Swap 2                     -- [q, 8*r, mask, i, ...]
    , M.Push memBeginning
    , M.IAdd                       -- [memBeginning+q, 8*r, mask, i, ...]
    , M.Dup 0                      -- [memBeginning+q, memBeginning+q, 8*r, mask, i, ...]
    , M.MemLoad Nothing            -- [v, memBeginning+q, 8*r, mask, i, ...]
    , M.Swap 1, M.Swap 3           -- [mask, v, 8*r, memBeginning+q, i, ...]
    , M.IAnd                       -- [v', 8*r, memBeginning+q, i, ...]
    , M.Swap 3                     -- [i, 8*r, memBeginning+q, v', ...]
    , M.Swap 1                     -- [8*r, i, memBeginning+q, v', ...]
    , M.IShL                       -- [i', memBeginning+q, v', ...]
    , M.Swap 1, M.Swap 2           -- [v', i', memBeginning+q, ...]
    , M.IOr                        -- [final_val, memBeginning+q, ...]
    , M.Swap 1                     -- [memBeginning+q, final_val, ...]
    , M.MemStore Nothing           -- [...]
    ]
translateInstr (W.I32Load16U (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  typed [W.I32] [W.I32] $>
    [ M.Push (fromIntegral offset) -- [offset, byte_addr, ...]
    , M.IAdd                       -- [byte_addr+offset, ...]
    , M.IDivMod (Just 4)           -- [r, q, ...]
                                  -- where byte_addr+offset = 4*q + r
    , M.Swap 1                     -- [q, r, ...]
    , M.Push memBeginning
    , M.IAdd                       -- [memBeginning+q, r, ...]
    , M.MemLoad Nothing            -- [v, r, ...]
    , M.Swap 1                     -- [r, v, ...]
    -- we have an i32 (v), but we need just the 16 bits between spots 8*r and 8*r+15
    -- so we AND with the right mask and shift the result right by 8*r bits.
    -- (v & mask) << (8*r) gives us (as an i32) the value of the 16-bits starting
    -- at position 8*r. e.g (with lowest bits on the left):
    -- v    = xxxxxxxx|abcdefgh|ijklmnop|xxxxxxxx
    -- mask = 00000000|11111111|11111111|00000000
    -- and  = 00000000|abcdefgh|ijklmnop|00000000
    -- res  = abcdefgh|ijklmnop|00000000|00000000
    -- note: 11111111|11111111 is 65535
    , M.Push 8, M.IMul       -- [8*r, v, ...]
    , M.Swap 1, M.Dup 1      -- [8*r, v, 8*r, ...]
    , M.Push 65535, M.Swap 1 -- [8*r, 65535, v, 8*r...]
    , M.IShL                 -- [mask, v, 8*r, ...]
    , M.IAnd                 -- [and, 8*r, ...]
    , M.Swap 1, M.IShR       -- [res, ...]
    ]
translateInstr (W.I32Store16 (W.MemArg offset _align)) = do
  when (mod offset 4 == 3) $ error "offset = 3!"
  memBeginning <- getMemBeginning
  typed [W.I32, W.I32] [] $>
    [ M.Swap 1                     -- [byte_addr, i, ...]
    , M.Push (fromIntegral offset) -- [offset, byte_addr, i, ...]
    , M.IAdd                       -- [byte_addr+offset, i, ...]
    , M.IDivMod (Just 4)           -- [r, q, i, ...]
                                  -- where byte_addr+offset = 4*q + r
    , M.Push 8, M.IMul             -- [8*r, q, i, ...]
    , M.Dup 0                      -- [8*r, 8*r, q, i, ...]
    , M.Push 65535, M.Swap 1       -- [8*r, 65535, 8*r, q, i, ...]
    , M.IShL, M.INot               -- [mask, 8*r, q, i, ...]
    , M.Swap 2                     -- [q, 8*r, mask, i, ...]
    , M.Push memBeginning
    , M.IAdd                       -- [memBeginning+q, 8*r, mask, i, ...]
    , M.Dup 0                      -- [memBeginning+q, memBeginning+q, 8*r, mask, i, ...]
    , M.MemLoad Nothing            -- [v, memBeginning+q, 8*r, mask, i, ...]
    , M.Swap 1, M.Swap 3           -- [mask, v, 8*r, memBeginning+q, i, ...]
    , M.IAnd                       -- [v', 8*r, memBeginning+q, i, ...]
    , M.Swap 3                     -- [i, 8*r, memBeginning+q, v', ...]
    , M.Swap 1                     -- [8*r, i, memBeginning+q, v', ...]
    , M.IShL                       -- [i', memBeginning+q, v', ...]
    , M.Swap 1, M.Swap 2           -- [v', i', memBeginning+q, ...]
    , M.IOr                        -- [final_val, memBeginning+q, ...]
    , M.Swap 1                     -- [memBeginning+q, final_val, ...]
    , M.MemStore Nothing           -- [...]
    ]
-- locals
translateInstr (W.GetLocal k) = do
  localAddrs <- getLocalAddrs
  case Map.lookup k localAddrs of
    Just (loct, is) -> typed [] [loct] $> map M.LocLoad is
    _ -> error ("impossible: local variable " ++ show k ++ " not found?!")

translateInstr (W.SetLocal k) = do
  localAddrs <- getLocalAddrs
  case Map.lookup k localAddrs of
    Just (loct, as) -> typed [loct] [] $> map M.LocStore (reverse as)
    _ -> error ("impossible: local variable " ++ show k ++ " not found?!")
translateInstr (W.TeeLocal k) = do
  (<>) <$> translateInstr (W.SetLocal k)
        <*> translateInstr (W.GetLocal k)

-- globals
translateInstr (W.GetGlobal k) = do
  addr <- getGlobalAddr k
  getGlobalTy k >>= \case
    W.I32 -> typed [] [W.I32] $>
        [ M.MemLoad $ Just addr]
    W.I64 -> typed [] [W.I64] $>
        [ M.MemLoad $ Just addr
        , M.MemLoad . Just $ addr + 1
        ]
    t -> error $ "unsupported type: " ++ show t
translateInstr (W.SetGlobal k) = do
  addr <- getGlobalAddr k
  getGlobalTy k >>= \case
    W.I32 -> typed [W.I32] [] $>
        [ M.MemStore . Just $ addr ]
    W.I64 -> typed [W.I64] [] $>
        [ M.MemStore . Just $ addr + 1
        , M.MemStore . Just $ addr
        ]
    t -> error $ "unsupported type: " ++ show t

-- https://maticnetwork.github.io/miden/user_docs/stdlib/math/u64.html
-- 64 bits integers are emulated by separating the high and low 32 bits.
translateInstr (W.I64Const k) = typed [] [W.I64] $>
    [ M.Push k_lo
    , M.Push k_hi
    ]
  where FakeW64 k_hi k_lo = toFakeW64 k
translateInstr (W.I64Load (W.MemArg offset _align)) = do
  when (mod offset 4 /= 0) $ error "i64 load"
  memBeginning <- getMemBeginning
  -- we need to turn [byte_addr, ...] of wasm into
  -- [u32_addr, ...] for masm, and then call mem_load
  -- twice (once at u32_addr, once at u32_addr+1)
  -- to get lo and hi 32 bits of i64 value respectively.
  --
  -- u32_addr = (byte_addr / 4) + (offset / 4) + memBeginning
  typed [W.I32] [W.I64] $>
    [ M.Push 4, M.IDiv
    , M.Push (fromIntegral offset `div` 4)
    , M.IAdd
    , M.Push memBeginning, M.IAdd -- [addr, ...]
    , M.Dup 0 -- [addr, addr, ...]
    , M.MemLoad Nothing -- [lo, addr, ...]
    , M.Swap 1 -- [addr, lo, ...]
    , M.Push 1, M.IAdd -- [addr+1, lo, ...]
    , M.MemLoad Nothing -- [hi, lo, ...]
    ]
translateInstr (W.I64Store (W.MemArg offset _align)) = do
  when (mod offset 4 /= 0) $ error "i64 store"
  memBeginning <- getMemBeginning
  -- we need to turn [val_hi, val_low, byte_addr, ...] of wasm into
  -- [u32_addr, val64_hi, val64_low, ...] for masm,
  -- and the call mem_store twice
  -- (once at u32_addr, once at u32_addr+1)
  -- to get hi and lo 32 bits of i64 value into memory.
  typed [W.I64, W.I32] [] $>
      [ M.Swap 1, M.Swap 2 -- [byte_addr, hi, lo, ...]
      , M.Push 4, M.IDiv
      , M.Push (fromIntegral offset `div` 4)
      , M.IAdd
      , M.Push memBeginning
      , M.IAdd -- [addr, hi, lo, ...]
      , M.Dup 0 -- [addr, addr, hi, lo, ...]
      , M.Swap 2, M.Swap 1 -- [addr, hi, addr, lo, ...]
      , M.Push 1, M.IAdd -- [addr+1, hi, addr, lo, ...]
      , M.MemStore Nothing -- [addr, lo, ...]
      , M.MemStore Nothing -- [...]
      ]
translateInstr (W.I64Load8U (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  typed [W.I32] [W.I64] $>         -- [byte_addr, ...]
    [ M.Push (fromIntegral offset) -- [offset, byte_addr, ...]
    , M.IAdd
    , M.IDivMod (Just 8)           -- [r, q, ...]
                                  --   where byte_addr+offset = 8*q + r
    , M.Dup 0, M.Push 3, M.IGt     -- [r > 3, r, q, ...]
    , M.MoveUp 2, M.Push 2, M.IMul -- [2*q, r > 3, r, ...]
    , M.Push memBeginning          -- [memBeginning, 2*q, r > 3, r, ...]
    , M.IAdd                       -- [memBeginning+2*q, r > 3, r, ...]
    , M.IAdd                       -- [memBeginning+2*q+(r > 3), r, ...]
    , M.MemLoad Nothing            -- [v, r, ...]
    , M.Swap 1                     -- [r, v, ...]
    , M.Push 4, M.IMod             -- [r `mod` 4, v, ...]
    , M.Push 8, M.IMul             -- [8*(r `mod` 4), v, ...]
    , M.IShR                       -- [v', ...]
    , M.Push 255, M.IAnd           -- [v'', ...]
    , M.Push 0                     -- [0, v'', ...]
      -- the 8 bits we care about are the 8 lowest of the 32 bits v'' value here,
      -- and they need to be the 8 lowest bits of the 0-everywhere-else 64 bits result,
      -- so we shift and zero out the appropriate 32 bits we read from memory and push
      -- 0 on top to make it represent a 64 bits value
    ]
translateInstr (W.I64Store8 (W.MemArg offset _align)) = do
  memBeginning <- getMemBeginning
  -- we have an 8-bit value stored in an i64 (two 32 bits in Miden land),
  -- e.g (lowest on the left):
  -- i   = abcdefgh|00000000|00000000|00000000||00000000|00000000|00000000|00000000
  -- there's an i64 value stored at addr q, e.g:
  -- v   = xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx||xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx
  -- and we want to update the 8*r to 8*r+7 bits of v with
  -- the first 8 bits of i, so in the example ending with:
  -- res = xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx||xxxxxxxx|abcdefgh|xxxxxxxx|xxxxxxxx
  -- we get there by shifting i by 8*r bits to the "left":
  -- i'  = 00000000|00000000|00000000|00000000||00000000|abcdefgh|00000000|00000000
  -- setting free the relevant bits in v:
  -- v'  = xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx||xxxxxxxx|00000000|xxxxxxxx|xxxxxxxx
  -- and storing v' | i'
  --
  -- TODO: only load, transform and store the limb that's targetted by the memory
  -- op?
  typed [W.I64, W.I32] [] $>        -- [i_hi, i_lo, byte_addr, ...]
    [ M.MoveUp 2                   -- [byte_addr, i_hi, i_lo, ...]
    , M.Push (fromIntegral offset) -- [offset, byte_addr, i_hi, i_lo, ...]
    , M.IAdd                       -- [byte_addr+offset, i_hi, i_lo, ...]
    , M.IDivMod (Just 8)           -- [r, q, i_hi, i_lo, ...]
                                  -- where byte_addr+offset = 8*q + r

    , M.Push 8, M.IMul             -- [8*r, q, i_hi, i_lo, ...]
    , M.Push 255, M.Push 0         -- [0, 255, 8*r, q, i_hi, i_lo, ...]
    , M.Dup 2                      -- [8*r, 0, 255, 8*r, q, i_hi, i_lo, ...]

    , M.IShL64                     -- [(255 << 8*r)_hi, (255 << 8*r)_lo, 8*r, q, i_hi, i_lo, ...]
    , M.INot                       -- [mask_hi, (255 << 8*r)_lo, 8*r, q, i_hi, i_lo, ...]
    , M.Swap 1, M.INot, M.Swap 1   -- [mask_hi, mask_lo, 8*r, q, i_hi, i_lo, ...]

    , M.MoveUp 2
    , M.MoveUp 3                   -- [q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
    , M.Push 2, M.IMul             -- [2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]

    , M.Push memBeginning          -- [memBeginning, 2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
    , M.IAdd                       -- [memBeginning+2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
    , M.Dup 0, M.Dup 0             -- [memBeginning+2*q, memBeginning+2*q, memBeginning+2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
    , M.MemLoad Nothing            -- [v_lo, memBeginning+2*q, memBeginning+2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]

    , M.Swap 1, M.Push 1, M.IAdd   -- [memBeginning+2*q+1, v_lo, memBeginning+2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
    , M.MemLoad Nothing            -- [v_hi, v_lo, memBeginning+2*q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]

    , M.MoveUp 5, M.MoveUp 5       -- [mask_hi, mask_lo, v_hi, v_lo, memBeginning+2*q, 8*r, i_hi, i_lo, ...]
    , M.IAnd64                     -- [v'_hi, v'_lo, memBeginning+2*q, 8*r, i_hi, i_lo, ...]

    , M.MoveUp 5, M.MoveUp 5       -- [i_hi, i_lo, v'_hi, v'_lo, memBeginning+2*q, 8*r, ...]
    , M.MoveUp 5                   -- [8*r, i_hi, i_lo, v'_hi, v'_lo, memBeginning+2*q, ...]
    , M.IShL64                     -- [i'_hi, i'_lo, v'_hi, v'_lo, memBeginning+2*q, ...]

    , M.IOr64                      -- [res_hi, res_lo, memBeginning+2*q, ...]

    , M.Dup 2, M.Push 1, M.IAdd    -- [memBeginning+2*q+1, res_hi, res_lo, memBeginning+2*q, ...]
    , M.MemStore Nothing           -- [res_lo, memBeginning+2*q, ...]
    , M.Swap 1                     -- [memBeginning+2*q, res_lo, ...]
    , M.MemStore Nothing           -- [...]
    ]

-- turning an i32 into an i64 in wasm corresponds to pushing 0 on the stack.
-- let's call the i32 'i'. before executing this, the stack looks like [i, ...],
-- and after like: [0, i, ...].
-- Since an i64 'x' on the stack is in Miden represented as [x_hi, x_lo], pushing 0
-- effectively grabs the i32 for the low bits and sets the high 32 bits to 0.
translateInstr W.I64ExtendUI32 = typed [W.I32] [W.I64] $> [M.Push 0]
-- similarly, wrap drops the high 32 bits, which amounts to dropping the tip of the stack
-- in miden, going from [v_hi, v_lo, ...] to [v_lo, ...]
translateInstr W.I32WrapI64 = typed [W.I64] [W.I32] $> [M.Drop]
-- this is a sign-aware extension, so we push 0 or maxBound :: Word32
-- depending on whether the 32 bits number is negative or not.
translateInstr W.I64ExtendSI32 = typed [W.I32] [W.I64] $>
    -- TODO: investigate performance of computing both branches and using select.
    --       Miden docs suggest it might be faster.
    [ M.Dup 0                  -- [x, x, ...]
    ] ++ computeIsNegative ++  -- [x_negative, x, ...]
    [ M.If
        [ M.Push maxBound      -- [0b11..1, x, ...]
        ]
        [ M.Push 0             -- [0, x, ...]
        ]
    ]

translateInstr W.I64Eqz = typed [W.I64] [W.I32] $> [M.IEqz64]
-- is the top of the WASM stack an i32 or i64, at this point in time?
-- i32 => 1 MASM 'drop', i64 => 2 MASM 'drop's.
translateInstr W.Drop = withPrefix $ pure . \t -> replicate (numCells t) M.Drop

-- We always have 2³² memory addresses, or more than the current Wasm we're targeting supports.
translateInstr W.CurrentMemory = typed [] [W.I32] $> [M.Push 0xFFFF]
-- Return -1 to indicate that we were unable to grow memory.
translateInstr W.GrowMemory = typed [W.I32] [W.I32] $> [M.Push 0xFFFFFFFF]

translateInstr i = unsupportedInstruction i

-- Function indices are checked by the wasm library and will always be in range.
importedFunctionTypeIndex :: W.Import -> W.TypeIndex
importedFunctionTypeIndex W.Import { desc = W.ImportFunc idx } = idx
importedFunctionTypeIndex i = error $ "This import is not a function: " <> show i

getFunctionType :: Integral fi => fi -> V W.FuncType
getFunctionType fi = (! fromIntegral fi) <$> getFuncTypes

blockType :: W.BlockType -> V W.FuncType
blockType (W.Inline Nothing) =
  pure W.FuncType {params = [], results = [] }
blockType (W.Inline (Just t)) =
  pure $ W.FuncType { params = [], results = [t] }
blockType (W.TypeIndex ti) = getType ti

stackFromBlockN :: Natural -> V W.ResultType
stackFromBlockN n = do
  -- TODO(Matthias): The logic here looks a bit weird, investigate.
  stack <- get
  asks ((stack <>) . f n)
  where f 0 _ = []
        f n' (InBlock _ _ s:ctxs) = if n' == 1 then s else s <> f (n'-1) ctxs
        f _ (InFunction {}:_) = []
        f n' (_:ctxs) = f n' ctxs
        f _ [] = []

blockNBranchType :: Natural -> V W.ResultType
blockNBranchType frames = f frames =<< ask
  where f :: Natural -> [Ctx] -> V W.ResultType
        f 0 (InBlock Block t _:_) = W.results <$> blockType t
        f 0 (InBlock Loop t _:_) = W.params <$> blockType t
        f n (InBlock {}:ctxs) = f (n-1) ctxs
        f _ (InFunction {funcId}:_) = W.results <$> getFunctionType funcId
        f n (_:ctxs) = f n ctxs
        f n [] = error $
          "Asked to go up " ++ show frames ++ " blocks, "
          ++ "but we only have " ++ show (frames - n) ++ " of context."


branch :: Natural -> V [M.Instruction]
branch idx = do
  -- Clean up the stack.
  stack <- stackFromBlockN idx
  t <- blockNBranchType idx
  let resultStackSize = sum $ fmap numCells t
      drop' = case resultStackSize of
                0 -> [M.Drop]
                1 -> [M.Swap 1, M.Drop]
                _ -> [M.MoveUp (fromIntegral resultStackSize), M.Drop]
  if resultStackSize >= M.accessibleStackDepth
    then bad $ BlockResultTooLarge resultStackSize
    else pure $ concat (replicate (length stack - resultStackSize) drop') <>
                -- Set the branch counter.
                [ M.Push (fromIntegral idx + 1)
                , M.MemStore (Just branchCounter)
                ]

continue
  :: Applicative f
  => W.Instruction Natural
  -> f [M.Instruction]
  -> f [M.Instruction]
continue instruction localExit
  | Set.singleton C.Local == exits = localExit
  | Set.notMember C.Local exits = pure []
  | otherwise = localExit <&> \rest ->
    [ M.MemLoad (Just branchCounter)
    , M.Eq (Just 1)
    , M.If [ M.Push 0
            , M.MemStore (Just branchCounter)
            ] []
    , M.MemLoad (Just branchCounter)
    , M.NEq (Just 0)
    , M.If [ M.MemLoad (Just branchCounter)
            , M.Sub (Just 1)
            , M.MemStore (Just branchCounter)
            ]
            rest
    ]
  where exits = C.findExits instruction

continueLoop :: [M.Instruction]
continueLoop =
  [ M.MemLoad (Just branchCounter)
  , M.Eq (Just 1)
  , M.Dup 0
  , M.If [ M.Push 0
          , M.MemStore (Just branchCounter)
          ] []
  ]

translateIUnOp :: W.BitSize -> W.IUnOp -> V [M.Instruction]
translateIUnOp W.BS32 op = case op of
  W.IPopcnt -> typed [W.I32] [W.I32] $> [M.IPopcnt]
  _         -> unsupportedInstruction (W.IUnOp W.BS32 op)

translateIUnOp W.BS64 op = case op of
  W.IPopcnt -> typed [W.I64] [W.I64] $> -- [a, b, ...]
    [ M.IPopcnt                      -- [popcnt(a), b, ...]
    , M.Swap 1                       -- [b, popcnt(a), ...]
    , M.IPopcnt                      -- [popcnt(b), popcnt(a), ...]
    , M.IAdd                         -- [popcnt(a)+popcnt(b), ...]
    , M.Push 0                       -- [0, popcnt(a)+popcnt(b), ...]
    ]
  _         -> unsupportedInstruction (W.IUnOp W.BS64 op)


translateIBinOp :: W.BitSize -> W.IBinOp -> V [M.Instruction]
-- TODO: the u64 module actually provides implementations of many binops for 64 bits
-- values.
translateIBinOp W.BS64 op = case op of
  W.IAdd  -> stackBinop W.I64 M.IAdd64
  W.ISub  -> stackBinop W.I64 M.ISub64
  W.IMul  -> stackBinop W.I64 M.IMul64
  W.IDivU -> stackBinop W.I64 M.IDiv64
  W.IShl  -> typed [W.I64, W.I64] [W.I64] $> [M.Drop, M.IShL64]
  W.IShrU -> typed [W.I64, W.I64] [W.I64] $> [M.Drop, M.IShR64]
  W.IOr   -> stackBinop W.I64 M.IOr64
  W.IAnd  -> stackBinop W.I64 M.IAnd64
  W.IXor  -> stackBinop W.I64 M.IXor64

  W.IShrS ->
    typed [W.I64, W.I64] [W.I64] $> -- [b_hi, b_lo, a_hi, a_lo, ...]
    ( [ M.Drop                   -- [b_lo, a_hi, a_lo, ...]
      , M.Push 64, M.IMod        -- [b%64, a_hi, a_lo, ...]
      ] ++ computeDup64 1 ++     -- [a_hi, a_lo, b%64, a_hi, a_lo, ...]
      computeIsNegative64 ++     -- [a_negative, b%64, a_hi, a_lo, ...]
      [ M.If                     -- [b%64, a_hi, a_lo, ...]
          ( M.MoveDown 2         -- [a_hi, a_lo, b%64, ...]
            : computeNot64 ++    -- [~a_hi, ~a_lo, b%64, ...]
            [ M.MoveUp 2         -- [b%64, ~a_hi, ~a_lo, ...]
            , M.IShR64           -- [ (~a >> b%64)_hi, (~a >> b%64)_lo, ...]
            ] ++
            computeNot64         -- [(a >> b%64)_hi, (a >> b%64)_lo, ...]
          )
          [ M.IShR64 ]           -- [(a >> b%64)_hi, (a >> b%64)_lo, ...]
      ]
    )
  W.IDivS ->
    typed [W.I64, W.I64] [W.I64] $> -- [b_hi, b_lo, a_hi, a_lo, ...]
    ( [ M.Dup 3, M.Dup 3 ] ++
      computeAbs64 ++              -- [abs(a)_hi, abs(a)_lo, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.Dup 3, M.Dup 3 ] ++
      computeAbs64 ++              -- [abs(b)_hi, abs(b)_lo, abs(a)_hi, abs(a)_lo, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.IDiv64                   -- [(abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, b_hi, b_lo, a_hi, a_lo, ...]
      , M.MoveUp 5, M.MoveUp 5     -- [a_hi, a_lo, (abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, b_hi, b_lo, ...]
      ] ++ computeIsNegative64 ++  -- [a_negative, (abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, b_hi, b_lo, ...]
      [ M.MoveUp 4, M.MoveUp 4     -- [b_hi, b_lo, a_negative, (abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, ...]
      ] ++ computeIsNegative64 ++  -- [b_negative, a_negative, (abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, ...]
      [ M.IXor                     -- [a_b_diff_sign, (abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, ...]
      , M.If                       -- [(abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, ...]
          computeNegate64          -- [(-abs(a)/abs(b))_hi, (-abs(a)/abs(b))_lo, ...]
          []                       -- [(abs(a)/abs(b))_hi, (abs(a)/abs(b))_lo, ...]
      ]
    )
  W.IRemU -> stackBinop W.I64 M.IMod64
  W.IRemS -> do -- [b_hi, b_lo, a_hi, a_lo, ...] and we want a `rem` b
    dups <- typed [W.I64, W.I64] [W.I64, W.I64, W.I64, W.I64] $>
      (computeDup64 2 ++ computeDup64 2) -- [b_hi, b_lo, a_hi, a_lo, b_hi, b_lo, a_hi, a_lo, ...]
    divRes <- translateIBinOp W.BS64 W.IDivS -- [q_hi, q_lo, b_hi, b_lo, a_hi, a_lo, ...]
    remRes <- typed [W.I64, W.I64, W.I64] [W.I64] $>
      [ M.IMul64 -- [ (b*q)_hi, (b*q)_lo, a_hi, a_lo, ...]
      , M.ISub64 -- [ (a - b*q)_hi, (a - b*q)_lo, ...]
      ]
    return (dups ++ divRes ++ remRes)

  -- in both of these, the number of bits by which we "rotate" needs to be a 64 bits integer for wasm
  -- even though it's surely safe to assume we won't get arguments that don't fit in an 32 bits integer.
  -- but since Miden wants a 32 bits argument there we just drop the high limb of the 64 bits integer.
  W.IRotl -> typed [W.I64, W.I64] [W.I64] $> [M.Drop, M.IRotl64]
  W.IRotr -> typed [W.I64, W.I64] [W.I64] $> [M.Drop, M.IRotr64]
translateIBinOp W.BS32 op = case op of
  W.IAdd  -> stackBinop W.I32 M.IAdd
  W.ISub  -> stackBinop W.I32 M.ISub
  W.IMul  -> stackBinop W.I32 M.IMul
  W.IShl  -> stackBinop W.I32 M.IShL
  W.IShrU -> stackBinop W.I32 M.IShR
  W.IAnd  -> stackBinop W.I32 M.IAnd
  W.IOr   -> stackBinop W.I32 M.IOr
  W.IXor  -> stackBinop W.I32 M.IXor
  W.IRemU -> stackBinop W.I32 M.IMod
  W.IDivU -> stackBinop W.I32 M.IDiv

  -- https://bisqwit.iki.fi/story/howto/bitmath/#DviIdivDiviSignedDivision
  W.IDivS ->
    typed [W.I32, W.I32] [W.I32] $> -- [b, a, ...]
    ( [ M.Dup 1 ] ++ computeAbs ++ -- [abs(a), b, a, ...]
      [ M.Dup 1 ] ++ computeAbs ++ -- [abs(b), abs(a), b, a, ...]
      [ M.IDiv                     -- [abs(a)/abs(b), b, a, ...]
      , M.Swap 2                   -- [a, b, abs(a)/abs(b), ...]
      ] ++ computeIsNegative ++    -- [a_negative, b, abs(a)/abs(b), ...]
      [ M.Swap 1                   -- [b, a_negative, abs(a)/abs(b), ...]
      ] ++ computeIsNegative ++    -- [b_negative, a_negative, abs(a)/abs(b), ...]
      [ M.IXor                     -- [a_b_diff_sign, abs(a)/abs(b), ...]
      , M.If                       -- [abs(a)/abs(b), ...]
          computeNegate            -- [-abs(a)/abs(b), ...]
          []                       -- [abs(a)/abs(b), ...]
      ]
    )
  W.IRemS -> do -- [b, a, ...] and we want a `rem` b
    dups <- typed [W.I32, W.I32] [W.I32, W.I32, W.I32, W.I32] $>
      [M.Dup 1, M.Dup 1] -- [b, a, b, a, ...]
    divRes <- translateIBinOp W.BS32 W.IDivS -- [q, b, a, ...]
    remRes <- typed [W.I32, W.I32, W.I32] [W.I32] $>
      [ M.IMul -- [ b*q, a, ...]
      , M.ISub -- [ a - b*q, ...]
      ]
    return (dups ++ divRes ++ remRes)
  W.IShrS -> typed [W.I32, W.I32] [W.I32] $> -- [b, a, ...]
    ( [ M.Dup 1                  -- [a, b, a, ...]
      ] ++ computeIsNegative ++  -- [a_negative, b, a, ...]
      [ M.If                     -- [b, a, ...]
          [ M.Swap 1, M.INot     -- [~a, b, ...]
          , M.Swap 1             -- [b, ~a, ...]
          , M.IShR               -- [~a >> b, ...]
          , M.INot               -- [~(~a >> b), ...]
          ]
          [ M.IShR ]            -- [ a >> b, ...]
      ]
    )
  W.IRotl -> stackBinop W.I32 M.IRotl
  W.IRotr -> stackBinop W.I32 M.IRotr

translateIRelOp :: W.BitSize -> W.IRelOp -> V [M.Instruction]
translateIRelOp W.BS64 op = case op of
  W.IEq  -> stackRelop W.I64 M.IEq64
  W.INe  -> stackRelop W.I64 M.INeq64
  W.ILtU -> stackRelop W.I64 M.ILt64
  W.IGtU -> stackRelop W.I64 M.IGt64
  W.ILeU -> stackRelop W.I64 M.ILte64
  W.IGeU -> stackRelop W.I64 M.IGte64
  W.ILtS -> typed [W.I64, W.I64] [W.I32] $>     -- [b_hi, b_lo, a_hi, a_lo, ...]
    ( [ M.Dup 3, M.Dup 3                        -- [a_hi, a_lo, b_hi, b_lo, a_hi, a_lo, ...]
      ] ++ computeIsNegative64 ++               -- [a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.Dup 2, M.Dup 2                        -- [b_hi, b_lo, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      ] ++ computeIsNegative64 ++               -- [b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.Dup 1, M.Dup 1, M.IXor                -- [a_b_diff_sign, b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      , M.If                                    -- [b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
          -- different sign
          [ M.If                                -- [a_neg, b_hi, b_lo, a_hi, a_lo, ...]
              -- b negative
              [ M.Drop, M.Drop, M.Drop
              , M.Drop, M.Drop                  -- [...]
              , M.Push 0                        -- [0, ...] (a < b is false, because a >= 0 and b < 0)
              ]
              -- a negative
              [ M.Drop, M.Drop, M.Drop
              , M.Drop, M.Drop                  -- [...]
              , M.Push 1                        -- [1, ...] (a < b is true, because a < 0 and b >= 0)
              ]
          ]
          -- same sign
          [ M.Drop, M.Drop                      -- [b_hi, b_lo, a_hi, a_lo, ...]
          , M.ILt64                             -- [a < b, ...]
          ]
      ]
    )
  W.IGtS -> typed [W.I64, W.I64] [W.I32] $>     -- [b_hi, b_lo, a_hi, a_lo, ...]
    ( [ M.Dup 3, M.Dup 3                        -- [a_hi, a_lo, b_hi, b_lo, a_hi, a_lo, ...]
      ] ++ computeIsNegative64 ++               -- [a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.Dup 2, M.Dup 2                        -- [b_hi, b_lo, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      ] ++ computeIsNegative64 ++               -- [b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      [ M.Dup 1, M.Dup 1, M.IXor                -- [a_b_diff_sign, b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
      , M.If                                    -- [b_neg, a_neg, b_hi, b_lo, a_hi, a_lo, ...]
          -- different sign
          [ M.If                                -- [a_neg, b_hi, b_lo, a_hi, a_lo, ...]
              -- b negative
              [ M.Drop, M.Drop, M.Drop
              , M.Drop, M.Drop                  -- [...]
              , M.Push 1                        -- [1, ...] (a > b is true, because a >= 0 and b < 0)
              ]
              -- a negative
              [ M.Drop, M.Drop, M.Drop
              , M.Drop, M.Drop                  -- [...]
              , M.Push 0                        -- [0, ...] (a > b is false, because a < 0 and b >= 0)
              ]
          ]
          -- same sign
          [ M.Drop, M.Drop                      -- [b_hi, b_lo, a_hi, a_lo, ...]
          , M.IGt64                             -- [a > b, ...]
          ]
      ]
    )
  W.ILeS -> do
    -- TODO(Matthias): clean this up, it's a bit confusing.
    unsignedGtInstrs <-
      withLocalStack [W.I64, W.I64] $
      translateIRelOp W.BS64 W.IGtS
    typed [W.I64, W.I64] [W.I32] $>  -- [b_hi, b_lo, a_hi, a_lo, ...]
      ( unsignedGtInstrs ++          -- [a > b, ...]
        [M.Push 1, M.IXor]           -- [a <= b, ...]
      )
  W.IGeS -> do
    -- TODO(Matthias): clean this up, it's a bit confusing.
    unsignedLtInstrs <- withLocalStack [W.I64, W.I64] $
      translateIRelOp W.BS64 W.ILtS
    typed [W.I64, W.I64] [W.I32] $>  -- [b_hi, b_lo, a_hi, a_lo, ...]
      ( unsignedLtInstrs ++          -- [a < b, ...]
        [M.Push 1, M.IXor]           -- [a >= b, ...]
      )
translateIRelOp W.BS32 op = case op of
  W.IEq  -> stackRelop W.I32 (M.IEq Nothing)
  W.INe  -> stackRelop W.I32 M.INeq
  W.ILtU -> stackRelop W.I32 M.ILt
  W.IGtU -> stackRelop W.I32 M.IGt
  W.ILeU -> stackRelop W.I32 M.ILte
  W.IGeU -> stackRelop W.I32 M.IGte
  W.ILtS -> typed [W.I32, W.I32] [W.I32] $>     -- [b, a, ...]
    ( [ M.Dup 1
      ] ++ computeIsNegative ++                 -- [a_neg, b, a, ...]
      [ M.Dup 1
      ] ++ computeIsNegative ++                 -- [b_neg, a_neg, b, a, ...]
      [ M.Dup 1, M.Dup 1, M.IXor                -- [a_b_diff_sign, b_neg, a_neg, b, a, ...]
      , M.If                                    -- [b_neg, a_neg, b, a, ...]
          -- different sign
          [ M.If                                -- [a_neg, b, a, ...]
              -- b negative
              [ M.Drop, M.Drop, M.Drop          -- [...]
              , M.Push 0                        -- [0, ...] (a < b is false, because a >= 0 and b < 0)
              ]
              -- a negative
              [ M.Drop, M.Drop, M.Drop          -- [...]
              , M.Push 1                        -- [1, ...] (a < b is true, because a < 0 and b >= 0)
              ]
          ]
          -- same sign
          [ M.Drop, M.Drop                      -- [b, a, ...]
          , M.ILt                               -- [a < b, ...]
          ]
      ]
    )
  W.IGtS -> typed [W.I32, W.I32] [W.I32] $>     -- [b, a, ...]
    ( [ M.Dup 1
      ] ++ computeIsNegative ++                 -- [a_neg, b, a, ...]
      [ M.Dup 1
      ] ++ computeIsNegative ++                 -- [b_neg, a_neg, b, a, ...]
      [ M.Dup 1, M.Dup 1, M.IXor                -- [a_b_diff_sign, b_neg, a_neg, b, a, ...]
      , M.If                                    -- [b_neg, a_neg, b, a, ...]
          -- different sign
          [ M.If                                -- [a_neg, b, a, ...]
              -- b negative
              [ M.Drop, M.Drop, M.Drop          -- [...]
              , M.Push 1                        -- [1, ...] (a > b is true, because a >= 0 and b < 0)
              ]
              -- a negative
              [ M.Drop, M.Drop, M.Drop          -- [...]
              , M.Push 0                        -- [0, ...] (a > b is false, because a < 0 and b >= 0)
              ]
          ]
          -- same sign
          [ M.Drop, M.Drop                      -- [b, a, ...]
          , M.IGt                               -- [a > b, ...]
          ]
      ]
    )
  W.ILeS -> do
    -- TODO(Matthias): clean this up, it's a bit confusing.
    unsignedGtInstrs <- withLocalStack [W.I32, W.I32] $
      translateIRelOp W.BS32 W.IGtS
    typed [W.I32, W.I32] [W.I32] $>  -- [b, a, ...]
      ( unsignedGtInstrs ++          -- [a > b, ...]
        [M.Push 1, M.IXor]           -- [a <= b, ...]
      )
  W.IGeS -> do
    -- TODO(Matthias): clean this up, it's a bit confusing.
    unsignedLtInstrs <- withLocalStack [W.I32, W.I32] $
      translateIRelOp W.BS32 W.ILtS
    typed [W.I32, W.I32] [W.I32] $>  -- [b, a, ...]
      ( unsignedLtInstrs ++          -- [a < b, ...]
        [M.Push 1, M.IXor]           -- [a >= b, ...]
      )

checkTypes :: [W.ValueType] -> V [W.ValueType]
checkTypes = traverse f
  where f W.I32 = pure W.I32
        f W.I64 = pure W.I64
        f t     = unsupportedArgType t

stackBinop :: W.ValueType -> M.Instruction -> V [M.Instruction]
stackBinop ty xs = typed [ty, ty] [ty] $> [xs]

stackRelop :: W.ValueType -> M.Instruction -> V [M.Instruction]
stackRelop ty xs = typed [ty, ty] [W.I32] $> [xs]

-- TODO: turn those into procedures?

computeAbs :: [M.Instruction]
computeAbs =           -- [x, ...]
  [ M.Dup 0 ] ++       -- [x, x, ...]
  computeIsNegative ++ -- [x_highest_bit, x, ...]
  [ M.If               -- [x, ...]
      computeNegate    -- [-x, ...]
      []               -- [x, ...]
  ]

computeAbs64 :: [M.Instruction]
computeAbs64 =           -- [x_hi, x_lo, ...]
  computeDup64 0 ++      -- [x_hi, x_lo, x_hi, x_lo, ...]
  computeIsNegative64 ++ -- [x_negative, x_hi, x_lo, ...]
  [ M.If                 -- [x_hi, x_lo, ...]
      computeNegate64    -- [(-x)_hi, (-x)_lo, ...]
      []                 -- [x_hi, x_lo, ...]
  ]

-- negate a number using two's complement encoding:
-- 4294967295 = 2^32-1 is the largest Word32
-- 4294967295 + 1 wraps around to turn into 0
-- so 4294967295 - x + 1 "is indeed" -x, but computing
-- the subtraction first and then adding one is a very concise way
-- to negate a number using two's complement.
computeNegate :: [M.Instruction]
computeNegate =       -- [x, ...]
  [ M.Push hi         -- [4294967295, x, ...]
  , M.Swap 1, M.ISub  -- [4294967295 - x, ...]
  , M.Push 1, M.IAdd  -- [4294967295 - x + 1, ...]
  ]
  where hi = maxBound

computeNegate64 :: [M.Instruction]
computeNegate64 =       -- [x_hi, x_lo, ...]
  [ M.Push max_lo
  , M.Push max_hi       -- [max_hi, max_lo, x_hi, x_lo, ...]
  , M.MoveUp 3
  , M.MoveUp 3          -- [x_hi, x_lo, max_hi, max_lo, ...]
  , M.ISub64            -- [(max-x)_hi, (max-x)_lo, ...]
  , M.Push 1, M.Push 0  -- [0, 1, (max-x)_hi, (max-x)_lo, ...]
  , M.IAdd64            -- [(max - x + 1)_hi, (max - x + 1)_lo, ...]
  ]
  where FakeW64 max_hi max_lo = toFakeW64 maxBound

computeIsNegative :: [M.Instruction]
computeIsNegative = -- [x, ...]
  [ M.Push hi       -- [2^31-1, x, ...]
  , M.IGt           -- [x > 2^31-1, ...] (meaning it's a two's complement encoded negative integer)
  ]
  where hi = 2^(31::Int) - 1

computeIsNegative64 :: [M.Instruction]
computeIsNegative64 =   -- [a_hi, a_lo, ...]
  [ M.Push neglimit_lo  -- [l_lo, a_hi, a_lo, ...]
  , M.Push neglimit_hi  -- [l_hi, l_lo, a_hi, a_lo, ...]
  , M.IGt64             -- [a > l, ...]
  ]
  where FakeW64 neglimit_hi neglimit_lo = toFakeW64 (2^(63::Int) - 1)

computeNot64 :: [M.Instruction]
computeNot64 = -- [a_hi, a_lo, ...]
  [ M.INot     -- [~a_hi, a_lo, ...]
  , M.Swap 1   -- [a_lo, ~a_hi, ...]
  , M.INot     -- [~a_lo, ~a_hi, ...]
  , M.Swap 1
  ]

computeDup64 :: Word32 -> [M.Instruction]
computeDup64 i = -- [..., a_hi, a_lo, ...] limbs at indices i and i+1
  [ M.Dup (i+1)      -- [a_lo, ..., a_hi, a_lo, ...]
  , M.Dup (i+1)      -- [a_hi, a_lo, ..., a_hi, a_lo, ...]
  ]

typed :: W.ParamsType -> W.ResultType -> V ()
typed params result = do
  stack <- get
  case stripPrefix params stack of
    Nothing -> bad (ExpectedStack params stack)
    Just stack' -> put $ result <> stack'

typedF :: W.FuncType -> V ()
typedF W.FuncType {params, results} = typed params results

withPrefix :: (W.ValueType -> V a) -> V a
withPrefix f = get >>= \ case
  [] -> bad EmptyStack
  x:xs -> put xs >> f x

writeW32s :: [Word8] -> [M.Instruction]
writeW32s [] = []
writeW32s (a:b:c:d:xs) =
  let w = foldl' (.|.) 0 [ shiftL (fromIntegral x) (8 * i)
                          | (i, x) <- zip [0..] [a,b,c,d]
                          ]
  in [ M.Dup 0 -- [addr_u32, addr_u32, ...]
      , M.Push w -- [w, addr_u32, addr_u32, ...]
      , M.Swap 1 -- [addr_u32, w, addr_u32, ...]
      , M.MemStore Nothing -- [addr_u32, ...]
      , M.Push 1, M.IAdd -- [addr_u32+1, ...]
      ] ++ writeW32s xs
writeW32s xs = writeW32s $ xs ++ replicate (4-length xs) 0

-- TODO: define one procedure per type of function, this way we'd minimize the cost of
--       all indirect calls by having dedicated binary searches on smaller trees?
mkStarkifyCallIndirect :: [W.ElemSegment] -> V M.Proc
mkStarkifyCallIndirect elems = inContext CallIndirectFun $ do
  instrs <- genInstrs elems
  return (M.Proc 1 instrs)

  where segmentFuns segment@(W.ElemSegment tableIdx offsetExpr funIds)
          | tableIdx /= 0 = badNoMultipleTable
          | otherwise = case offsetExpr of
              [W.I32Const offset] -> return $ zip [offset..] funIds
              _                   -> unsupportedElemDynOffset segment
        genInstrs segments = do
          funs <- sortOn fst . concat <$> traverse segmentFuns segments
          guardAllConsecutive funs
          return $ binarySearchInstrs $ (fmap.fmap) procName funs

        guardAllConsecutive ((i, fi):(j, fj):xs)
          | j == i+1  = guardAllConsecutive ((j, fj):xs)
          | otherwise = badFunsNotConsecutive i fi j fj
        guardAllConsecutive _ = return ()

binarySearchInstrs :: [(Word32, T.Text)] -> [M.Instruction]
binarySearchInstrs = go
  where go [] = []
        go [(_i, name)] = [ M.Drop, M.Exec name ]
        go funs =
          let midpoint = length funs `div` 2
              midfun@(mid, name) = funs !! midpoint in
                          -- [fun_off, ...args...]
          [ M.Push mid    -- [mid, fun_off, ...args...]
          , M.Dup 1       -- [fun_off, mid, fun_off, ...args...]
          , M.IEq Nothing -- [fun_off == mid, fun_off, ...args...]
          , M.If          -- [fun_off, ...args...]
              [ M.Drop    -- [...args...]
              , M.Exec name
              ]
              (if length funs > 2
                then [ M.Dup 0    -- [fun_off, fun_off, ...args...]
                     , M.Push mid -- [mid, fun_off, fun_off, ...args...]
                     , M.ILt      -- [fun_off < mid, fun_off, ...args...]
                     , M.If       -- [fun_off, ...args...]
                         (go (take midpoint funs))
                         (go (drop (midpoint+1) funs))
                     ]
                else go (filter (/=midfun) funs)
              )
          ]

-- utilities

(!) :: HasCallStack => Vector a -> Int -> a
(!) = (V.!)
