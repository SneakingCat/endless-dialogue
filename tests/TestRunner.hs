module Main (main) where

import System.Process.EndlessDialogue
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
  testGroup "Dialogue smoke tests" [
     testCase "Maybe listen shall be nothing if no data" nothingIfNoDataTest
     ]
  ]
        
nothingIfNoDataTest :: Assertion
nothingIfNoDataTest = do
  d <- open "./Echo" []
  v <- maybeListen d
  assertEqual "Shall be Nothing" v Nothing
