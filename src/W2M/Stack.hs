module W2M.Stack where

import Control.Monad.Except
import Control.Monad.State
import Data.List (isPrefixOf)

type StackFun ctx a = StateT StackType (Either (StackProblem ctx)) a
data StackProblem ctx
  = StackExpectedGot [StackElem] StackType ctx
  | StackEmpty ctx
  deriving Show
type StackType = [StackElem]
data StackElem = SI32 | SI64
  deriving (Eq, Show)

-- withStack :: ctx -> StackType -> 

assumingPrefix
 :: ctx -> StackType -> (StackType -> (a, StackType)) -> StackFun ctx a
assumingPrefix ctx prefix f = do
  stack <- get
  if prefix `isPrefixOf` stack
    then case f (drop (length prefix) stack) of
           (a, stack') -> put stack' >> return a
    else throwError (StackExpectedGot prefix stack ctx)

noPrefix :: (StackType -> (a, StackType)) -> StackFun ctx a
noPrefix f = state f

withPrefix :: ctx -> (StackElem -> StackFun ctx a) -> StackFun ctx a
withPrefix ctx f = do
  stack <- get
  case stack of
    [] -> throwError (StackEmpty ctx)
    x:xs -> put xs >> f x

foldStackFuns :: StackType -> [StackFun ctx a] -> Either (StackProblem ctx) ([a], StackType)
foldStackFuns stackTy0 fs = runStateT (sequence fs) stackTy0