{-# LANGUAGE TypeApplications #-}
module Properties where

import Data.Int
import Data.Word
import System.IO ( hSetBuffering, stdout, BufferMode(..) )
import Test.Hspec ( hspec, describe )
import Test.Hspec.QuickCheck ( prop )
import Properties.Arith (arithTest)
import Properties.Cmp (cmpTest)
import Properties.Memory (memStoreTest, memLoadTest)

-- * Testsuite driver

-- | Set the following to True for printing WASM/MASM modules, expressions, results, etc
dbg :: Bool
dbg = False
-- dbg = True

-- To run more examples, use something like
-- cabal test --test-option=--qc-max-success=10000 arith-test
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hspec $ do
    describe "starkify translates arithmetic operations correctly" $ do
      prop "for unsigned 32 bits integers" $
        arithTest @Word32 dbg
      prop "for signed 32 bits integers" $
        arithTest @Int32 dbg
      prop "for unsigned 64 bits integers" $
        arithTest @Word64 dbg
      prop "for signed 64 bits integer" $
        arithTest @Int64 dbg
    describe "starkify translates comparison operators correctly" $ do
      prop "for unsigned 32 bits integers" $
        cmpTest @Word32 dbg
      prop "for signed 32 bits integers" $
        cmpTest @Int32 dbg
      prop "for unsigned 64 bits integers" $
        cmpTest @Word64 dbg
      prop "for signed 64 bits integer" $
        cmpTest @Int64 dbg
    describe "starkify translates memory operations correctly" $ do
      describe "i32.store family" $ do
        prop "i32.store" $
          memStoreTest @Word32 @Word32 dbg
        prop "i32.store8" $
          memStoreTest @Word32 @Word8 dbg
        prop "i32.store16" $
          memStoreTest @Word32 @Word16 dbg
      describe "i32.load family" $ do
        prop "i32.load" $
          memLoadTest @Word32 @Word32 dbg
        prop "i32.load8_u" $
          memLoadTest @Word32 @Word8 dbg
        prop "i32.load8_s" $
          memLoadTest @Word32 @Int8 dbg
        prop "i32.load16_u" $
          memLoadTest @Word32 @Word16 dbg
      describe "i64.store family" $ do
        prop "i64.store" $
          memStoreTest @Word64 @Word64 dbg
        prop "i64.store8" $
          memStoreTest @Word64 @Word8 dbg
      describe "i64.load family" $ do
        prop "i64.load" $
          memLoadTest @Word64 @Word64 dbg
        prop "i64.load8_u" $
          memLoadTest @Word64 @Word8 dbg
