{-# LANGUAGE OverloadedStrings #-}

module Count where

import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Map qualified as DM
import Data.Text (Text, pack, words)
import Data.Text.IO (putStrLn)
import GHC.Natural (Natural)
import Language.Wasm (decodeLazy)
import Language.Wasm.Structure
import Text.Printf (printf)
import Prelude hiding (putStrLn, unwords, words)

runCount :: FilePath -> IO ()
runCount fp = do
  r <- decodeLazy <$> LBS.readFile fp
  case r of
    Left e -> error $ "Decoding error: " ++ show e
    Right wasm_mod -> do
      let counts = sortOn (negate . snd) $ DM.toList $ countM wasm_mod
          p (k, v) = putStrLn $ pack (printf "% 10d" v ++ " ") <> k
      traverse_ p counts

countM :: Module -> DM.Map Text Int
countM Module {functions} =
  DM.fromListWith (+) $ do
    Function {body} <- functions
    instruction <- body
    name <- count1 instruction
    return (name, 1)

count1 :: Instruction Natural -> [Text]
count1 = \case
  Block _ body -> "Block" : (count1 =<< body)
  Loop _ body -> "Loop" : (count1 =<< body)
  If _ true false -> "If" : (count1 =<< true) ++ (count1 =<< false)
  x -> take 1 . words . pack $ show x
