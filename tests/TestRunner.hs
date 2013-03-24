module Main (main) where

import System.Process.EndlessDialogue
import qualified Data.ByteString.Char8 as BS
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import System.Timeout

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
  testGroup "Dialogue smoke tests" [
     testCase "Listen shall timeout if no data" timeoutIfNoData
     , testCase "Listen shall return what echo is told" asEchoIsTold
     , testCase "Shall follow conversation" followConversation
     , testCase "Shall follow bursty conversation" followBurstyConversation
     ]
  ]
        
timeoutIfNoData :: Assertion
timeoutIfNoData = do
  d <- open echoProgram []
  v <- timeout shortTimeout $ listen d
  assertEqual "Shall be nothing" v Nothing
  
asEchoIsTold :: Assertion
asEchoIsTold = do
  d <- open echoProgram []
  let s = echoString 0
  tell d s
  s' <- listen d
  assertEqual "Shall be equal" s s'
  
followConversation :: Assertion  
followConversation = do
  d <- open echoProgram []
  let ss = map echoString [0..100]
  mapM_ (talk d) ss
  where
    talk :: Dialogue -> BS.ByteString -> Assertion
    talk d s = do
      tell d s
      listen d >>= assertEqual "Shall be equal" s
  
followBurstyConversation :: Assertion
followBurstyConversation = do
  d <- open echoProgram []
  let ss = map echoString [0..100]
  mapM_ (tell d) ss
  mapM_ (listenAndTest d) ss
  where
    listenAndTest :: Dialogue -> BS.ByteString -> Assertion
    listenAndTest d s = do
      listen d >>= assertEqual "Shall be equal" s
  
echoProgram :: String
echoProgram = "./dist/build/Echo/Echo"

shortTimeout :: Int
shortTimeout = 1000 * 50 -- 50 ms

echoString :: Int -> BS.ByteString
echoString n = BS.pack $ "A string of text #" ++ show n