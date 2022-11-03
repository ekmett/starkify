{-# LANGUAGE OverloadedStrings #-}

module Count where

import Data.ByteString.Lazy qualified as LBS
import Data.Foldable
import Data.Function (on)
import Data.List (sortOn)
import Data.Map qualified as DM
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Text hiding (take)
import Data.Text.IO
import GHC.Natural (Natural)
import Language.Wasm (decodeLazy)
import Language.Wasm.Structure
import Language.Wasm.Structure qualified as WASM
import Language.Wasm.Validate qualified as WASM
import Text.Pretty.Simple
import Text.Printf
import Prelude hiding (putStrLn, unwords, words)

deriving instance Ord (Instruction Natural)

deriving instance Ord BlockType

deriving instance Ord ValueType

deriving instance Ord MemArg

deriving instance Ord BitSize

deriving instance Ord IUnOp

deriving instance Ord IBinOp

deriving instance Ord IRelOp

deriving instance Ord FUnOp

deriving instance Ord FBinOp

deriving instance Ord FRelOp

(<+>) :: (Semigroup a, Ord k) => DM.Map k a -> DM.Map k a -> DM.Map k a
(<+>) = DM.unionWith (<>)

combine :: (Semigroup a, Ord k) => [DM.Map k a] -> DM.Map k a
combine = DM.unionsWith (<>)

runCount :: FilePath -> IO ()
runCount fp = do
  r <- decodeLazy <$> LBS.readFile fp
  case r of
    Left e -> error $ "Decoding error: " -- ++ show e
    Right wasm_mod -> do
      let counts = sortOn (negate . snd) $ DM.toList $ countM wasm_mod
          p (k, v) = putStrLn $ pack (printf "% 6d" v ++ " ") <> k
      traverse_ p $ counts

countM :: Module -> DM.Map Text Int
countM Module {functions} =
  fmap getSum $
    DM.unionsWith (<>) $
      fmap countF functions

countF :: Function -> DM.Map Text (Sum Int)
countF Function {body} = DM.unionsWith (<>) $ fmap count1 body

count1 :: Instruction Natural -> DM.Map Text (Sum Int)
count1 = \case
  Block _ body -> DM.singleton "Block" (Sum 1) <+> combine (fmap count1 body)
  Loop _ body -> DM.singleton "Loop" (Sum 1) <+> combine (fmap count1 body)
  If _ true false ->
    DM.singleton "If" (Sum 1)
      <+> combine (fmap count1 true)
      <+> combine (fmap count1 false)
  x -> DM.singleton (unwords . take 1 . words . pack $ show x) (Sum 1)
