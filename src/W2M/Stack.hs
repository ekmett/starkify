module W2M.Stack where

import Control.Monad.Except
import Control.Monad.State
import GHC.Natural
import qualified Language.Wasm.Structure as W
import Data.List (isPrefixOf)

type StackFun = StateT StackType (Either StackProblem)
data StackProblem = StackExpectedGot [StackElem] StackType (W.Instruction Natural)
  deriving Show
type StackType = [StackElem]
data StackElem = SI32 | SI64
  deriving (Eq, Show)

assumingPrefix
 :: W.Instruction Natural -> StackType -> (StackType -> (a, StackType)) -> StackFun a
assumingPrefix inst prefix f = do
  stack <- get
  if prefix `isPrefixOf` stack
    then case f (drop (length prefix) stack) of
           (a, stack') -> put stack' >> return a
    else throwError (StackExpectedGot prefix stack inst)

noPrefix :: (StackType -> (a, StackType)) -> StackFun a
noPrefix f = state f

withPrefix :: (StackElem -> StackFun a) -> StackFun a
withPrefix f = do
  stack <- get
  case stack of
    [] -> error "withPrefix: empty stack"
    x:xs -> put xs >> f x

foldStackFuns :: StackType -> [StackFun a] -> Either StackProblem ([a], StackType)
foldStackFuns stackTy0 fs = runStateT (sequence fs) stackTy0