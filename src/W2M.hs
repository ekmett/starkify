{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module W2M where

import Data.Bifunctor (first)
import Control.Applicative
import Control.Monad.Except
import Data.Bits
import Data.ByteString.Lazy qualified as BS
import Data.Foldable
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Word (Word8, Word32)
import GHC.Natural (Natural)
import Language.Wasm.Structure qualified as W

import MASM qualified as M
import MASM.Interpreter (toFakeW64, FakeW64 (..))
import Tools (dfs)
import Validation
import W2M.Stack (StackElem(..), StackType, StackProblem(..), StackFun)
import W2M.Stack qualified as WStack

import Data.Text.Lazy qualified  as T
import Control.Monad.Reader

type WasmAddr = Natural
type MasmAddr = Word32
type LocalAddrs = Map WasmAddr (StackElem, [MasmAddr])
type FunId = Natural
type FunName = Text

data WasmImportedFun = WIF
  { wifModule :: Text
  , wifName   :: Text
  , wifTy     :: W.FuncType
  } deriving (Eq, Show)

toMASM :: Bool -> W.Module -> V M.Module
toMASM checkImports m = do
  -- TODO: don't throw away main's type, we might want to check it and inform how the program can be called?
  when checkImports $ inContext ImportsCheck $ checkImps (W.imports m)
  globalsInit <- inContext GlobalsInit getGlobalsInit
  datasInit <- inContext DatasInit getDatasInit

  M.Module ["std::sys", "std::math::u64"]
    <$> fmap catMaybes (traverse fun2MASM sortedFuns)
    <*> return (M.Program (globalsInit ++ datasInit ++ [ M.Exec "main" ] ++ stackCleanUp))

  where checkImps
          = traverse_ (\(W.Import imodule iname idesc) -> badImport imodule iname $ descType idesc)

        descType idesc = case idesc of
          W.ImportFunc _t -> "function"
          W.ImportTable _t -> "table"
          W.ImportMemory _l -> "memory"
          W.ImportGlobal _g -> "global"

        globalsAddrMap :: Vector MasmAddr
        memBeginning :: MasmAddr
        (globalsAddrMap, memBeginning) = first V.fromList $ foldl' f ([], 0) (W.globals m)
          where f (xs, n) globl_i =
                  let ncells = case W.globalType globl_i of
                                 W.Const t -> numCells t
                                 W.Mut   t -> numCells t
                  in (xs ++ [n], n+ncells)

        callGraph :: Map FunId (Set FunId)
        callGraph = Map.fromListWith (<>) $
          [ (caller, Set.singleton callee)
          | (caller, (_mname, Left (W.Function {body}))) <- Map.toList allFunctionsMap
          , W.Call callee <- body
          ]
        -- enumerate x = x : concatMap enumerate [ y | y <- maybe [] Set.toList (Map.lookup x callGraph) ]
        mainFunId
          | Just (W.StartFunction k) <- W.start m = k
          | Just mainId <- Map.lookup "main" functionIdsMap = mainId
          | otherwise = error "No start function in WASM module and no 'main', cannot proceed."

        sortedFuns :: [(FunId, Maybe FunName, W.Function)]
        sortedFuns = reverse $ catMaybes $ dfs mainFunId callGraph <&> \name ->
          case Map.lookup name allFunctionsMap of
                    Just (mname, Left f) -> Just (name, mname, f)
                    -- TODO(Matthias): Do we need to handle problems more carefully here?
                    _ -> Nothing

        numCells :: W.ValueType -> Word32
        numCells t = case t of
          W.I32 -> 1
          W.I64 -> 2
          _ -> error "numCells called on non integer value type"

        getDatasInit :: V [M.Instruction]
        getDatasInit = concat <$> traverse getDataInit (W.datas m)

        getDataInit :: W.DataSegment -> V [M.Instruction]
        getDataInit (W.DataSegment 0 offset_wexpr bytes) = do
          offset_mexpr <- translateInstrs [] mempty offset_wexpr
          pure $ offset_mexpr ++
                 [ M.Push 4, M.IDiv             -- [offset_bytes/4, ...]
                 , M.Push memBeginning, M.IAdd  -- [offset_bytes/4+memBeginning, ...] =
                 ] ++                           -- [addr_u32, ...]
                 writeW32s (BS.unpack bytes) ++ -- [addr_u32+len(bytes)/4, ...]
                 [ M.Drop ]                     -- [...]
        getDataInit _ = badNoMultipleMem

        getGlobalsInit :: V [M.Instruction]
        getGlobalsInit = concat <$> traverse getGlobalInit (zip [0..] (W.globals m))

        getGlobalInit :: (Int, W.Global) -> V [M.Instruction]
        getGlobalInit (k, g) =
          translateInstrs [] mempty (W.initializer g ++ [W.SetGlobal $ fromIntegral k])

        getGlobalTy k
          | fromIntegral k < length (W.globals m) = case t of
              W.I32 -> SI32
              W.I64 -> SI64
              _     -> error "unsupported global type"
          | otherwise = error "getGlobalTy: index too large"

            where t = case W.globalType (W.globals m !! fromIntegral k) of
                        W.Const ct -> ct
                        W.Mut mt -> mt

        importedName modl nm = modl <> "." <> nm
        importedFuns = [ (mdl, nm, W.types m !! fromIntegral tyidx)
                       | W.Import mdl nm (W.ImportFunc tyidx) <- W.imports m
                       ]

        allFuns :: [(FunId, Maybe FunName, Either W.Function WasmImportedFun)]
        allFuns =
          zipWith (\k (mdl, nm, t) -> (k, Just (importedName mdl nm), Right $ WIF mdl nm t))
                  [0..]
                  importedFuns ++
          [ (fromIntegral k, lkpFunName (fromIntegral k), Left f)
          | (k, f) <- zip [(length importedFuns)..] (W.functions m)
          ]

        lkpFunName i = go (W.exports m)
          where go [] = Nothing
                go (e:es) = case e of
                  W.Export name (W.ExportFunc j)
                    | i == j -> Just name
                  _ -> go es

        allFunctionsMap :: Map FunId (Maybe FunName, Either W.Function WasmImportedFun)
        allFunctionsMap = Map.fromList [ (i, (n, f)) | (i, n, f) <- allFuns ]

        emptyFunctions :: Set FunId
        emptyFunctions = Set.fromList
          [ fid
          | (fid, (_, Left (W.Function _ _ []))) <- Map.toList allFunctionsMap
          ]

        functionNamesMap :: Map FunId FunName
        functionNamesMap = Map.fromList
          [ (i, fname)
          | (i, Just fname, _t) <- allFuns
          ]

        functionIdsMap :: Map FunName FunId
        functionIdsMap = Map.fromList . map swap . Map.toList $ functionNamesMap
          where swap (a, b) = (b, a)

        functionTypesMap :: Map FunId W.FuncType
        functionTypesMap = allFunctionsMap <&> \case
                  (_, Left (W.Function tyIdx _ _)) -> W.types m !! fromIntegral tyIdx
                  (_, Right (WIF _mdl _nm t)) -> t

        fixName t
          | Just t' <- T.stripPrefix "__" t = "zz__" <> t'
          | otherwise                       = t

        fun2MASM :: (FunId, Maybe Text, W.Function) -> V (Maybe M.Proc)
        fun2MASM (_funid, _mfname, W.Function _         _         []) = return Nothing
        fun2MASM (funid, mfname, W.Function _funTyIdx localsTys body) = inContext (InFunction mfname (fromIntegral funid)) $ do
          let wasm_args = maybe [] W.params (Map.lookup funid functionTypesMap)
              wasm_locals = localsTys

              localAddrMap :: LocalAddrs
              (localAddrMap, nlocalCells) =
                foldl' (\(addrs, cnt) (k, ty) -> case ty of
                           W.I32 -> (Map.insert k (SI32, [cnt]) addrs, cnt+1)
                           W.I64 -> (Map.insert k (SI64, [cnt, cnt+1]) addrs, cnt+2)
                           _     -> error "localAddrMap: floating point local var?"
                       )
                       (Map.empty, 0)
                       (zip [0..] (wasm_args ++ wasm_locals))
              -- the function starts by populating the first nargs local vars
              -- with the topmost nargs values on the stack, removing them from
              -- the stack as it goes. it assumes the value for the first arg
              -- was pushed first, etc, with the value for the last argument
              -- being pushed last and therefore popped first.
              prelude = reverse $ concat
                [ case Map.lookup (fromIntegral k) localAddrMap of
                    Just (_t, is) -> concat [ [ M.Drop, M.LocStore i ] | i <- is ]
                    _ -> error ("impossible: prelude of procedure " ++ show (funid, mfname) ++ ", local variable " ++ show k ++ " not found?!")
                | k <- [0..(length wasm_args - 1)]
                ]

          instrs <- translateInstrs [] localAddrMap body
          return $ Just (M.Proc (fromIntegral funid) (fmap fixName mfname) (fromIntegral nlocalCells) (prelude ++ instrs))


        translateInstrs :: StackType -> LocalAddrs -> W.Expression -> V [M.Instruction]
        translateInstrs stack0 locals wasmInstrs = do
          -- we do this in two steps so that we can report e.g unsupported instructions
          -- or calls to unexisting functions "applicatively" and add some additional sequential
          -- reporting logic that tracks the type of the stack and allows 'translateInstr' to
          -- inspect the stack to bail out if they're not right and otherwise
          -- use the types of the stack values to drive the MASM code generation.
          stackFuns <- sequenceA $
            zipWith (\k inst -> inContext (InInstruction k inst) $ translateInstr locals inst) [1..] wasmInstrs
          case WStack.foldStackFuns stack0 stackFuns of
            Left e@(StackExpectedGot _ _ ctxs) -> withContexts ctxs (badStackTypeError e)
            Left e@(StackEmpty ctxs) -> withContexts ctxs (badStackTypeError e)
            Right (a, _finalT) -> return (concat a)
            -- TODO: make sure finalT is the function's return type?

        translateInstr :: LocalAddrs -> W.Instruction Natural -> V (StackFun [Ctx] [M.Instruction])
        translateInstr _ (W.Call i) = case Map.lookup i functionTypesMap of
          Just (W.FuncType params res) -> do
              let mname = Map.lookup (fromIntegral i) functionNamesMap
              params' <- checkTypes params
              res' <- checkTypes res
              let procname = maybe (T.pack $ "func" ++ show i) fixName mname
                  instrs =
                    if Set.member i emptyFunctions
                      then concat [ if t == SI64 then [ M.Drop, M.Drop ] else [ M.Drop ]
                                  | t <- params'
                                  ]
                      else [M.Exec procname]
              assumingPrefix (reverse params') $ \t -> (instrs, res' ++ t)
          Nothing -> badWasmFunctionCallIdx (fromIntegral i)
        translateInstr _ (W.I32Const w32) = noPrefix $ \t -> ([M.Push w32], SI32:t)
        translateInstr _ (W.IBinOp bitsz op) = translateIBinOp bitsz op
        translateInstr _ W.I32Eqz =
          assumingPrefix [SI32] $ \t -> ([M.IEq (Just 0)], SI32:t)
        translateInstr _ (W.IRelOp bitsz op) = translateIRelOp bitsz op
        translateInstr _ W.Select =
          assumingPrefix [SI32, SI32, SI32] $ \t ->
            ([M.If True [M.Drop] [M.Swap 1, M.Drop]], SI32:t)
        translateInstr _ (W.I32Load (W.MemArg offset _align)) =
            assumingPrefix [SI32] $ \t ->
            -- assumes byte_addr is divisible by 4 and ignores remainder... hopefully it's always 0?
                 ( [ M.Push 4
                   , M.IDiv
                   , M.Push (fromIntegral offset `div` 4)
                   , M.IAdd
                   , M.Push memBeginning
                   , M.IAdd
                   , M.MemLoad Nothing
                   ]
                 , SI32:t
                 )
        translateInstr _ (W.I32Store (W.MemArg offset _align)) =
          -- we need to turn [val, byte_addr, ...] of wasm into [u32_addr, val, ...]
            assumingPrefix [SI32, SI32]
            -- assumes byte_addr is divisible by 4 and ignores remainder... hopefully it's always 0?
                 ( [ M.Swap 1
                   , M.Push 4
                   , M.IDiv
                   , M.Push (fromIntegral offset `div` 4)
                   , M.IAdd
                   , M.Push memBeginning
                   , M.IAdd
                   , M.MemStore Nothing
                   , M.Drop
                   ]
                 ,)
        translateInstr _ (W.I32Load8U (W.MemArg offset _align)) =
            assumingPrefix [SI32] $ \t ->
                 ( [ M.Push (fromIntegral offset) -- [offset, byte_addr, ...]
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
                 , SI32:t
                 )
        translateInstr a (W.I32Load8S mem) = do
          loadInstrs <- translateInstr a (W.I32Load8U mem)
          sigInstrs <- assumingPrefix [SI32] $ \t -> -- [v, ...]
                 ( [ M.Dup 0                         -- [v, v, ...]
                   , M.Push 128, M.IGte              -- [v >= 128, v, ...]
                   , M.If True                       -- [v, ...]
                       [ M.Push 255, M.Swap 1        -- [v, 255, ...]
                       , M.ISub, M.Push 1, M.IAdd    -- [255 - v + 1, ...]
                       , M.Push 4294967295           -- [4294967295, 255 - v + 1, ...]
                       , M.Swap 1, M.ISub            -- [4294967295 - (255 - v + 1), ...]
                       , M.Push 1, M.IAdd            -- [4294967295 - (255 - v + 1) + 1, ...]
                       ]
                       [] -- if the 32 bits of v encode a positive 8 bits number, nothing to do
                   ]
                 , SI32:t
                 )
          return $ (++) <$> loadInstrs <*> sigInstrs
        translateInstr _ (W.I32Store8 (W.MemArg offset _align)) =
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
            assumingPrefix [SI32, SI32]
                 ( [ M.Swap 1                     -- [byte_addr, i, ...]
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
                   , M.MemStore Nothing           -- [final_val, ...]
                   , M.Drop                       -- [...]
                   ]
                 ,)
        translateInstr _ (W.I32Load16U (W.MemArg offset _align)) =
            assumingPrefix [SI32] $ \t ->
                 ( [ M.Push (fromIntegral offset) -- [offset, byte_addr, ...]
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
                 , SI32:t
                 )
        translateInstr _ (W.I32Store16 (W.MemArg offset _align))
          | mod offset 4 == 3 = error "offset = 3!"
          | otherwise   =
            assumingPrefix [SI32, SI32]
                 ( [ M.Swap 1                     -- [byte_addr, i, ...]
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
                   , M.MemStore Nothing           -- [final_val, ...]
                   , M.Drop                       -- [...]
                   ]
                 ,)
        -- locals
        translateInstr localAddrs (W.GetLocal k) = case Map.lookup k localAddrs of
          Just (loct, is) -> noPrefix $ \t -> (map M.LocLoad is, loct:t)
          _ -> error ("impossible: local variable " ++ show k ++ " not found?!")

        translateInstr localAddrs (W.SetLocal k) = case Map.lookup k localAddrs of
          Just (loct, as) ->
            assumingPrefix [loct]
              ( concat
                  [ [ M.LocStore a
                    , M.Drop
                    ]
                  | a <- reverse as
                  ]
              ,)
          _ -> error ("impossible: local variable " ++ show k ++ " not found?!")
        translateInstr localAddrs (W.TeeLocal k) =
          liftA2 (++) <$> translateInstr localAddrs (W.SetLocal k)
                      <*> translateInstr localAddrs (W.GetLocal k)

        -- globals
        translateInstr _ (W.GetGlobal k) = case getGlobalTy k of
          SI32 -> noPrefix $ \t ->
            ( [ M.MemLoad . Just $ globalsAddrMap V.! fromIntegral k
              ]
            , SI32:t
            )
          SI64 -> noPrefix $ \t ->
            ( [ M.MemLoad . Just $ globalsAddrMap V.! fromIntegral k
              , M.MemLoad . Just $ (globalsAddrMap V.! fromIntegral k) + 1
              ]
            , SI64:t
            )
        translateInstr _ (W.SetGlobal k) = case getGlobalTy k of
          SI32 -> assumingPrefix [SI32]
            ( [ M.MemStore . Just $ globalsAddrMap V.! fromIntegral k
              , M.Drop
              ]
            ,)
          SI64 -> assumingPrefix [SI64]
            ( [ M.MemStore . Just $ (globalsAddrMap V.! fromIntegral k) + 1
              , M.Drop
              , M.MemStore . Just $ (globalsAddrMap V.! fromIntegral k)
              , M.Drop
              ]
            ,)

        -- https://maticnetwork.github.io/miden/user_docs/stdlib/math/u64.html
        -- 64 bits integers are emulated by separating the high and low 32 bits.
        translateInstr _ (W.I64Const k) = noPrefix $ \t ->
          ( [ M.Push k_lo
            , M.Push k_hi
            ]
          , SI64:t
          )
          where FakeW64 k_hi k_lo = toFakeW64 k
        translateInstr _ (W.I64Load (W.MemArg offset _align))
          | mod offset 4 /= 0 = error "i64 load"
          | otherwise         =
          -- we need to turn [byte_addr, ...] of wasm into
          -- [u32_addr, ...] for masm, and then call mem_load
          -- twice (once at u32_addr, once at u32_addr+1)
          -- to get lo and hi 32 bits of i64 value respectively.
          --
          -- u32_addr = (byte_addr / 4) + (offset / 4) + memBeginning
          assumingPrefix [SI32] $ \t ->
            ( [ M.Push 4, M.IDiv
              , M.Push (fromIntegral offset `div` 4)
              , M.IAdd
              , M.Push memBeginning, M.IAdd -- [addr, ...]
              , M.Dup 0 -- [addr, addr, ...]
              , M.MemLoad Nothing -- [lo, addr, ...]
              , M.Swap 1 -- [addr, lo, ...]
              , M.Push 1, M.IAdd -- [addr+1, lo, ...]
              , M.MemLoad Nothing -- [hi, lo, ...]
              ]
            , SI64:t
            )
        translateInstr _ (W.I64Store (W.MemArg offset _align))
          | mod offset 4 /= 0 = error "i64 store"
          | otherwise   =
          -- we need to turn [val_hi, val_low, byte_addr, ...] of wasm into
          -- [u32_addr, val64_hi, val64_low, ...] for masm,
          -- and the call mem_store twice
          -- (once at u32_addr, once at u32_addr+1)
          -- to get hi and lo 32 bits of i64 value.
          assumingPrefix [SI64, SI32]
            ( [ M.Swap 1, M.Swap 2 -- [byte_addr, hi, lo, ...]
              , M.Push 4, M.IDiv
              , M.Push (fromIntegral offset `div` 4)
              , M.IAdd
              , M.Push memBeginning
              , M.IAdd -- [addr, hi, lo, ...]
              , M.Dup 0 -- [addr, addr, hi, lo, ...]
              , M.Swap 2, M.Swap 1 -- [addr, hi, addr, lo, ...]
              , M.Push 1, M.IAdd -- [addr+1, hi, addr, lo, ...]
              , M.MemStore Nothing -- [hi, addr, lo, ...]
              , M.Drop -- [addr, lo, ...]
              , M.MemStore Nothing -- [lo, ...]
              , M.Drop -- [...]
              ]
            ,)
        translateInstr _ (W.I64Store8 (W.MemArg offset _align)) =
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
            assumingPrefix [SI32, SI64]           -- [i_hi, i_lo, byte_addr, ...]
                 ( [ M.Swap 1 , M.Swap 2          -- [byte_addr, i_hi, i_lo, ...]
                   , M.Push (fromIntegral offset) -- [offset, byte_addr, i_hi, i_lo, ...]
                   , M.IAdd                       -- [byte_addr+offset, i_hi, i_lo, ...]
                   , M.IDivMod (Just 4)           -- [r, q, i_hi, i_lo, ...]
                                                  -- where byte_addr+offset = 4*q + r
                   , M.Push 8, M.IMul             -- [8*r, q, i_hi, i_lo, ...]
                   , M.Dup 0                      -- [8*r, 8*r, q, i_hi, i_lo, ...]
                   , M.Push 255, M.Swap 1         -- [8*r, 255, 8*r, q, i_hi, i_lo, ...]
                   , M.IShL, M.INot               -- [mask_hi, mask_lo, 8*r, q, i_hi, i_lo, ...]
                   , M.Swap 2                     -- [8*r, mask_lo, mask_hi, q, i_hi, i_lo, ...]
                   , M.Swap 1                     -- [mask_lo, 8*r, mask_hi, q, i_hi, i_lo, ...]
                   , M.Swap 3                     -- [q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.Push memBeginning
                   , M.IAdd                       -- [memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.Dup 0, M.Dup 0             -- [memBeginning+q, memBeginning+q, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.MemLoad Nothing            -- [v_lo, memBeginning+q, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.Swap 1, M.Push 1, M.IAdd   -- [memBeginning+q+1, v_lo, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.MemLoad Nothing            -- [v_hi, v_lo, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.Dup 5, M.Dup 5             -- [mask_hi, mask_lo, v_hi, v_lo, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.IAnd64                     -- [v'_hi, v'_lo, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, i_lo, ...]
                   , M.Swap 7, M.Swap 1           -- [v'_lo, i_lo, memBeginning+q, 8*r, mask_hi, mask_lo, i_hi, v'_hi, ...]
                   , M.Swap 6                     -- [i_hi, i_lo, memBeginning+q, 8*r, mask_hi, mask_lo, v'_lo, v'_hi, ...]
                   , M.Dup 3, M.IShL64            -- [i'_hi, i'_lo, memBeginning+q, 8*r, mask_hi, mask_lo, v'_lo, v'_hi, ...]
                   , M.Swap 2, M.Swap 1           -- [i'_lo, memBeginning+q, i'_hi, 8*r, mask_hi, mask_lo, v'_lo, v'_hi, ...]
                   , M.Swap 3                     -- [8*r, memBeginning+q, i'_hi, i'_lo, mask_hi, mask_lo, v'_lo, v'_hi, ...]
                   , M.Swap 6, M.Swap 1           -- [memBeginning+q, v'_lo, i'_hi, i'_lo, mask_hi, mask_lo, 8*r, v'_hi, ...]
                   , M.Swap 7                     -- [v'_hi, v'_lo, i'_hi, i'_lo, mask_hi, mask_lo, 8*r, memBeginning+q, ...]
                   , M.IOr64                      -- [res_hi, res_lo, mask_hi, mask_lo, 8*r, memBeginning+q, ...]
                   , M.Dup 5                      -- [memBeginning+q, res_hi, res_lo, mask_hi, mask_lo, 8*r, memBeginning+q, ...]
                   , M.Push 1, M.IAdd             -- [memBeginning+q+1, res_hi, res_lo, mask_hi, mask_lo, 8*r, memBeginning+q, ...]
                   , M.MemStore Nothing, M.Drop   -- [res_lo, mask_hi, mask_lo, 8*r, memBeginning+q, ...]
                   , M.MoveUp 4                   -- [memBeginning+q, res_lo, mask_hi, mask_lo, 8*r, ...]
                   , M.MemStore Nothing, M.Drop   -- [mask_hi, mask_lo, 8*r, ...]
                   , M.Drop, M.Drop, M.Drop       -- [...]
                   ]
                 ,)
        -- TODO: ^^^^^^ use M.MoveUp more!


        -- turning an i32 into an i64 in wasm corresponds to pushing 0 on the stack.
        -- let's call the i32 'i'. before executing this, the stack looks like [i, ...],
        -- and after like: [0, i, ...].
        -- Since an i64 'x' on the stack is in Miden represented as [x_hi, x_lo], pushing 0
        -- effectively grabs the i32 for the low bits and sets the high 32 bits to 0.
        translateInstr _ W.I64ExtendUI32 = assumingPrefix [SI32] $ \t -> ([M.Push 0], SI64:t)
        -- similarly, wrap drops the high 32 bits, which amounts to dropping the tip of the stack
        -- in miden, going from [v_hi, v_lo, ...] to [v_lo, ...]
        translateInstr _ W.I32WrapI64 = assumingPrefix [SI64] $ \t -> ([M.Drop], SI32:t)
        -- this is a sign-aware extension, so we push 0 or maxBound :: Word32
        -- depending on whether the most significant bit of the i32 is 0 or 1.
        translateInstr _ W.I64ExtendSI32 = assumingPrefix [SI32] $ \t ->
          ( [ M.Dup 0                  -- [x, x, ...]
            , M.Push 2147483648        -- [2^31, x, x, ...]
            , M.IAnd                   -- [x & 2^31, x, ...]
            , M.Push 31, M.IShR        -- [x_highest_bit, x, ...]
            -- TODO: Use select
            , M.If True
                [ M.Push 4294967295    -- [0b11..1, x, ...]
                ]
                [ M.Push 0             -- [0, x, ...]
                ]
            ]
          , SI64:t
          )

        translateInstr _ W.I64Eqz = assumingPrefix [SI64] $ \t -> ([M.IEqz64], SI32:t)

        translateInstr _ W.Drop = withPrefix
          -- is the top of the WASM stack an i32 or i64, at this point in time?
          -- i32 => 1 MASM 'drop', i64 => 2 MASM 'drop's.
          \case
            SI32 -> return [M.Drop]
            SI64 -> return [M.Drop, M.Drop]

        translateInstr a i@(W.Block _ is) =
          inContext InBlock (translateInstrs [] a is) *>
          unsupportedInstruction i
        translateInstr a i@(W.Loop _ is) =
          inContext InLoop (translateInstrs [] a is) *>
          unsupportedInstruction i
        translateInstr _ i = unsupportedInstruction i

translateIBinOp :: W.BitSize -> W.IBinOp -> V (StackFun [Ctx] [M.Instruction])
-- TODO: the u64 module actually provides implementations of many binops for 64 bits
-- values.
translateIBinOp W.BS64 op = case op of
  W.IAdd  -> stackBinop SI64 M.IAdd64
  W.ISub  -> stackBinop SI64 M.ISub64
  W.IMul  -> stackBinop SI64 M.IMul64
  W.IShl  -> stackBinop SI64 M.IShL64
  W.IShrU -> stackBinop SI64 M.IShR64
  W.IOr   -> stackBinop SI64 M.IOr64
  W.IAnd  -> stackBinop SI64 M.IAnd64
  W.IXor  -> stackBinop SI64 M.IXor64
  _       -> unsupported64Bits op
translateIBinOp W.BS32 op = case op of
  W.IAdd  -> stackBinop SI32 M.IAdd
  W.ISub  -> stackBinop SI32 M.ISub
  W.IMul  -> stackBinop SI32 M.IMul
  W.IShl  -> stackBinop SI32 M.IShL
  W.IShrU -> stackBinop SI32 M.IShR
  W.IAnd  -> stackBinop SI32 M.IAnd
  W.IOr   -> stackBinop SI32 M.IOr
  W.IXor  -> stackBinop SI32 M.IXor
  W.IDivU -> stackBinop SI32 M.IDiv
  W.IDivS -> assumingPrefix [SI32, SI32] $ \t ->
                                   -- [b, a, ...]
    ( [ M.Dup 1 ] ++ computeAbs ++ -- [abs(a), b, a, ...]
      [ M.Dup 1 ] ++ computeAbs ++ -- [abs(b), abs(a), b, a, ...]
      [ M.IDiv                     -- [abs(a)/abs(b), b, a, ...]
      , M.Swap 2                   -- [a, b, abs(a)/abs(b), ...]
      ] ++ computeIsNegative ++    -- [a_negative, b, abs(a)/abs(b), ...]
      [ M.Swap 1                   -- [b, a_negative, abs(a)/abs(b), ...]
      ] ++ computeIsNegative ++    -- [b_negative, a_negative, abs(a)/abs(b), ...]
      [ M.IXor                     -- [a_b_diff_sign, abs(a)/abs(b), ...]
      , M.If True                  -- [abs(a)/abs(b), ...]
          computeNegate            -- [-abs(a)/abs(b), ...]
          []                       -- [abs(a)/abs(b), ...]
      ]
    , SI32 : t
    )
  _       -> unsupportedInstruction (W.IBinOp W.BS32 op)

translateIRelOp :: W.BitSize -> W.IRelOp -> V (StackFun [Ctx] [M.Instruction])
translateIRelOp W.BS64 op = case op of
  W.IEq  -> stackRelop SI64 M.IEq64
  W.INe  -> stackRelop SI64 M.INeq64
  W.ILtU -> stackRelop SI64 M.ILt64
  W.IGtU -> stackRelop SI64 M.IGt64
  W.ILeU -> stackRelop SI64 M.ILte64
  W.IGeU -> stackRelop SI64 M.IGte64
  _      -> unsupported64Bits op
translateIRelOp W.BS32 op = case op of
  W.IEq  -> stackRelop SI32 (M.IEq Nothing)
  W.INe  -> stackRelop SI32 M.INeq
  W.ILtU -> stackRelop SI32 M.ILt
  W.IGtU -> stackRelop SI32 M.IGt
  W.ILeU -> stackRelop SI32 M.ILte
  W.IGeU -> stackRelop SI32 M.IGte
  W.ILtS -> assumingPrefix [SI32, SI32] $ \t -> -- [b, a, ...]
    ( [ M.ISub                                  -- [a-b, ...]
      ] ++ computeIsNegative                    -- [a-b < 0, ...] =
                                                -- [a<b, ...]
    , SI32 : t
    )
  W.IGtS -> assumingPrefix [SI32, SI32] $ \t -> -- [b, a, ...]
    ( [ M.Swap 1                                -- [b-a, ...]
      , M.ISub                                  -- [b-a, ...]
      ] ++ computeIsNegative                    -- [b-a < 0, ...] =
                                                -- [b<a, ...]
    , SI32 : t
    )
  W.IGeS -> assumingPrefix [SI32, SI32] $ \t ->       -- [b, a, ...]
    ( [ M.Dup 0, M.Dup 2                              -- [b, a, b, a, ...]
      , M.IEq Nothing                                 -- [a == b, b, a, ...]
      , M.If True                                     -- [b, a, ...]
          [ M.Drop, M.Drop, M.Push 1 ]                -- [1, ...]
          ([ M.Swap 1, M.ISub ] ++ computeIsNegative) -- [a > b, ...]
      ]
    , SI32 : t
    )
  _      -> unsupportedInstruction (W.IRelOp W.BS32 op)

-- necessary because of https://github.com/maticnetwork/miden/issues/371
-- the stack must be left with exactly 16 entries at the end of the program
-- for proof generaton, so we remove a bunch of entries accordingly.
stackCleanUp :: [M.Instruction]
stackCleanUp = [] -- [M.TruncateStack]
-- stackCleanUp n = concat $ replicate n [ M.Swap n', M.Drop ]
--   where n' = fromIntegral n

checkTypes :: [W.ValueType] -> V [StackElem]
checkTypes = traverse f
  where f W.I32 = pure SI32
        f W.I64 = pure SI64
        f t     = unsupportedArgType t

stackBinop :: StackElem -> M.Instruction -> V (StackFun [Ctx] [M.Instruction])
stackBinop ty xs = assumingPrefix [ty, ty] $ \t -> ([xs], ty:t)

stackBinop' :: StackElem -> M.Instruction -> V (StackFun [Ctx] [M.Instruction])
stackBinop' ty xs = assumingPrefix [ty, ty] $ \t -> ([xs], ty:t)

stackRelop :: StackElem -> M.Instruction -> V (StackFun [Ctx] [M.Instruction])
stackRelop ty xs = assumingPrefix [ty, ty] $ \t -> ([xs], SI32:t)

-- TODO: turn those into procedures?

computeAbs :: [M.Instruction]
computeAbs =           -- [x, ...]
  [ M.Dup 0 ] ++       -- [x, x, ...]
  computeIsNegative ++ -- [x_highest_bit, x, ...]
  [ M.If True          -- [x, ...]
      computeNegate    -- [-x, ...]
      []               -- [x, ...]
  ]

computeNegate :: [M.Instruction]
computeNegate =       -- [x, ...]
  [ M.Push 4294967295 -- [4294967295, x, ...]
  , M.Swap 1, M.ISub  -- [4294967295 - x, ...]
  , M.Push 1, M.IAdd  -- [4294967295 - x + 1, ...]
  ]

computeIsNegative :: [M.Instruction]
computeIsNegative = -- [x, ...]
  [ M.Push hi       -- [2^31, x, ...]
  , M.IAnd          -- [2^31 & x, ...]
  , M.Push 31       -- [31, 2^31 & x, ...]
  , M.IShR          -- [x_highest_bit, ...]
  ]
  where hi = 2^(31::Int)

assumingPrefix :: StackType -> (StackType -> (a, StackType)) -> V (StackFun [Ctx] a)
assumingPrefix xs f = do
  ctxs <- ask
  return $ WStack.assumingPrefix ctxs xs f

withPrefix :: (StackElem -> StackFun [Ctx] a) -> V (StackFun [Ctx] a)
withPrefix f = do
  ctxs <- ask
  return $ WStack.withPrefix ctxs f

noPrefix :: (StackType -> (a, StackType)) -> V (StackFun ctx a)
noPrefix = pure . WStack.noPrefix

writeW32s :: [Word8] -> [M.Instruction]
writeW32s [] = []
writeW32s (a:b:c:d:xs) =
  let w = foldl' (.|.) 0 [ shiftL (fromIntegral x) (8 * i)
                          | (i, x) <- zip [0..] [a,b,c,d]
                          ]
  in [ M.Dup 0 -- [addr_u32, addr_u32, ...]
      , M.Push w -- [w, addr_u32, addr_u32, ...]
      , M.Swap 1 -- [addr_u32, w, addr_u32, ...]
      , M.MemStore Nothing -- [w, addr_u32, ...]
      , M.Drop -- [addr_u32, ...]
      , M.Push 1, M.IAdd -- [addr_u32+1, ...]
      ] ++ writeW32s xs
writeW32s xs = writeW32s $ xs ++ replicate (4-length xs) 0
