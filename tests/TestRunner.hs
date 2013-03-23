module Main (main) where

import System.Process.EndlessDialogue
import qualified Data.ByteString.Char8 as BS
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
  testGroup "Dialogue smoke tests" [
     testCase "Maybe listen shall be nothing if no data" nothingIfNoData
     , testCase "Listen shall return what echo is told" asEchoIsTold
     ]
  ]
        
nothingIfNoData :: Assertion
nothingIfNoData = do
  d <- open echoProgram []
  v <- maybeListen d
  assertEqual "Shall be Nothing" v Nothing
  
asEchoIsTold :: Assertion
asEchoIsTold = do
  d <- open echoProgram []
  let s = BS.pack "a string of text"
  tell d s
  s' <- listen d
  assertEqual "Shall be equal" s s'
  
echoProgram :: String
echoProgram = "./dist/build/Echo/Echo"