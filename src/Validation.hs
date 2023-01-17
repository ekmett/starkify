{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Validation where

import Control.Applicative
import Control.Monad.Validate
import Control.Monad.State
import Control.Monad.RWS.Strict
import Data.DList qualified as DList
import Data.Foldable
import Data.Function (on)
import Data.List (sortOn)
import Data.Typeable
import GHC.Natural
import GHC.Generics
import Language.Wasm.Structure qualified as W
import Language.Wasm.Validate qualified  as W

import Data.Text.Lazy qualified as LT
import Data.Word
import W2M.Common ( ModuleInfo(..) )

newtype Validation e a = Validation { getV :: ValidateT e (RWS [Ctx] () W.ResultType) a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadState W.ResultType (Validation e)
deriving instance (Semigroup e) => MonadValidate e (Validation e)
deriving instance MonadReader [Ctx] (Validation e)

instance Monoid e => Alternative (Validation e) where
  empty = refute mempty
  Validation a <|> Validation b = Validation $ do
    lift (runValidateT a) >>= \case
      Right ra -> return ra
      Left ea -> lift (runValidateT b) >>= \case
        Right rb -> return rb
        Left eb -> refute $ ea <> eb

bad :: e -> Validation (DList.DList (Error e)) a
bad e = do
  ctxs <- ask
  stack <- get
  refute [Error ctxs stack e]

data Block = Block | Loop | If deriving Show

data Ctx
  = InModule { moduleInfo :: ModuleInfo }
  | InFunction Int -- func id
  | GlobalsInit
  | DatasInit
  | Import
  | Typechecker
  | InInstruction Int (W.Instruction Natural) -- func id, instruction #
  | InBlock Block W.BlockType W.ResultType -- block kind, block type, parent block stack on block entry (the whole stack, not just the usable stack)
  | CallIndirectFun
  deriving Show

inContext :: Ctx -> Validation e a -> Validation e a
inContext c = local (c:)

getModuleInfo :: V ModuleInfo
getModuleInfo = do
  ctx <- ask
  let isInModule InModule {moduleInfo} = Just moduleInfo
      isInModule _ = Nothing
  case asum $ isInModule <$> ctx of
    Nothing -> bad NotInModule
    Just moduleInfo -> pure moduleInfo

blockDepth :: Validation e Int
blockDepth = length . filter isBlock <$> ask
  where isBlock (InBlock {}) = True
        isBlock _ = False

data ErrorData
  = FPOperation String
  | GlobalMut W.ValueType
  | NotInModule
  | NoMain
  | StdValidation W.ValidationError
  | UnsupportedInstruction (W.Instruction Natural)
  | Unsupported64Bits String
  | UnsupportedMemAlign Natural (W.Instruction Natural)
  | NoMultipleMem
  | UnsupportedImport LT.Text LT.Text LT.Text
  | ExpectedStack W.ParamsType W.ParamsType
  | UnsupportedArgType W.ValueType
  | EmptyStack
  | NamedGlobalRef LT.Text
  | BlockResultTooLarge Int
  | NoMultipleTable
  | UnsupportedElemDynOffset W.ElemSegment
  | UnsupportedNonConsecutiveFuns Word32 W.FuncIndex Word32 W.FuncIndex
  | BadStarkifyFun LT.Text
  deriving (Eq, Ord, Show)

deriving instance Ord (W.Instruction Natural)
deriving instance Ord W.ValueType
deriving instance Ord W.BlockType
deriving instance Ord W.MemArg
deriving instance Ord W.BitSize
deriving instance Ord W.IUnOp
deriving instance Ord W.IBinOp
deriving instance Ord W.IRelOp
deriving instance Ord W.FUnOp
deriving instance Ord W.FBinOp
deriving instance Ord W.FRelOp
deriving instance Ord W.ElemSegment

-- We need to do this manually, because Arrow ain't exported.
-- Otherwise, we could do:
-- deriving instance Ord W.Arrow
-- deriving instance Ord W.ValidationError
instance Ord W.ValidationError where
  compare = compare `on` show

data Error e = Error
  { errCtxs :: [Ctx]
  , errStack :: W.ResultType
  , errData :: e
  } deriving Show

badFPOp :: String -> V a
badFPOp s = bad (FPOperation s)

badGlobalMut :: W.ValueType -> V a
badGlobalMut t = bad (GlobalMut t)

badNoMain :: V a
badNoMain = bad NoMain

badNoMultipleMem :: V a
badNoMultipleMem = bad NoMultipleMem


badImport :: W.Import -> V a
badImport (W.Import imodule iname idesc) = bad (UnsupportedImport imodule iname (descType idesc))

badNoMultipleTable :: V a
badNoMultipleTable = bad NoMultipleTable

badNamedGlobalRef :: LT.Text -> V a
badNamedGlobalRef = bad . NamedGlobalRef

badStarkifyFun :: LT.Text -> V a
badStarkifyFun s = bad (BadStarkifyFun s)

descType :: W.ImportDesc -> LT.Text
descType idesc = case idesc of
                   W.ImportFunc _ -> "function"
                   W.ImportTable _ -> "table"
                   W.ImportMemory _ -> "memory"
                   W.ImportGlobal _ -> "global"

failsStandardValidation :: W.ValidationError -> V a
failsStandardValidation e = bad (StdValidation e)

unsupportedInstruction :: W.Instruction Natural -> V a
unsupportedInstruction i = bad (UnsupportedInstruction i)

unsupported64Bits :: Show op => op -> V a
unsupported64Bits op = bad (Unsupported64Bits $ show op)

unsupportedMemAlign :: Natural -> W.Instruction Natural -> V a
unsupportedMemAlign alig instr = bad (UnsupportedMemAlign alig instr)

unsupportedArgType :: W.ValueType -> V a
unsupportedArgType t = bad (UnsupportedArgType t)

unsupportedElemDynOffset :: W.ElemSegment -> V a
unsupportedElemDynOffset segment = bad (UnsupportedElemDynOffset segment)

badFunsNotConsecutive :: Word32 -> W.FuncIndex -> Word32 -> W.FuncIndex -> V a
badFunsNotConsecutive i fi j fj = bad (UnsupportedNonConsecutiveFuns i fi j fj)

ppErrData :: ErrorData -> String
ppErrData (FPOperation op) = "unsupported floating point operation: " ++ op
ppErrData (GlobalMut t) = "unsupported global mutable variable of type: " ++
  (case t of
     W.I32 -> "32 bits integer"
     W.I64 -> "64 bits integer"
     W.F32 -> "32 bits floating point"
     W.F64 -> "64 bits floating point"
  )
ppErrData NotInModule = "Trying to access module info when no module info has been set."
ppErrData NoMain = "No start function or 'main' function found in WASM module, cannot proceed."
ppErrData (StdValidation e) = "standard validator issue: " ++ show e
ppErrData (UnsupportedInstruction i) = "unsupported WASM instruction: " ++ show i
ppErrData (Unsupported64Bits opstr) = "unsupported 64 bit operation (" ++ opstr ++ ")"
ppErrData (UnsupportedMemAlign a instr) = "unsupported alignment: " ++ show a ++ " in " ++ show instr
ppErrData NoMultipleMem = "multiple memories not supported"
ppErrData (UnsupportedImport imodule iname idesc) =
  "unsupported import: module=" ++ LT.unpack imodule ++
  ", name=" ++ LT.unpack iname ++ " (" ++ LT.unpack idesc ++ ")"
ppErrData (ExpectedStack expected got) =
  "expected stack prefix " ++ show expected ++ " but got " ++ show (take (length expected) got)
ppErrData EmptyStack =
  "expected a non-empty stack"
ppErrData (UnsupportedArgType t) =
  "unsupported argument type: " ++ show t
ppErrData (NamedGlobalRef n) =
  "undefined global variable: " ++ show n
ppErrData (BlockResultTooLarge s) =
  "function result too large: " ++ show s
ppErrData NoMultipleTable = "multiple tables not supported"
ppErrData (UnsupportedElemDynOffset segment) =
  "unsupported dynamic offset for elem segment: " ++ show segment
ppErrData (UnsupportedNonConsecutiveFuns i fi j fj) =
  "non consecutive functions #" ++ show fi ++ " (offset " ++ show i ++ ") and #" ++
  show fj ++ " (offset " ++ show j ++ ")"
ppErrData (BadStarkifyFun n) = "unknown primitive starkify function " ++ show n

ppErr :: Error ErrorData -> [String]
ppErr e =
  [ red "error: " ++ ppErrData (errData e)
  ] ++
  [     "  ...  " ++ ppErrCtx c
  | c <- errCtxs e
  ] ++ [""]

  where red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"

ppErrCtx :: Ctx -> String
ppErrCtx (InModule _) = "in module"
ppErrCtx (InFunction i) = "of function " ++ show i
ppErrCtx DatasInit = "in data section"
ppErrCtx GlobalsInit = "in globals initialisation"
ppErrCtx Import = "in import"
ppErrCtx Typechecker = "in typechecking"
ppErrCtx (InInstruction k i) = "in instruction #" ++ show k ++ ": " ++ take 100 (show i) ++ "  ..."
ppErrCtx (InBlock t _ _) = "of " <> show t
ppErrCtx CallIndirectFun = "of generated indirect call function"

type V = Validation (DList.DList (Error ErrorData))

runValidation :: V a -> IO a
runValidation (Validation e) = case runRWS (runValidateT e) [] [] of
  (Left errs, _i, _w) -> error . unlines . toList .
    concatMap ppErr . sortOn errData $ toList errs
  (Right a, _i, _w) -> return a
