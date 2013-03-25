module Main (main) where

import System.Process.EndlessDialogue
import qualified Data.ByteString.Char8 as BS
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import System.Timeout
import Control.Concurrent (threadDelay)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
  testGroup "Dialogue smoke tests" [
     testCase "Listen shall block if no data" blockIfNoData1
     , testCase "Listen shall block if no data" blockIfNoData2
     , testCase "Listen all shall block if no data" blockIfNoData3
     , testCase "Listen shall return what echo is told" asEchoIsTold
     , testCase "Listen all shall return what echo is told" asEchoIsTold2
     , testCase "Shall follow conversation" followConversation
     , testCase "Shall follow bursty conversation" followBurstyConversation
     , testCase "Shall follow bursty conversation " followBurstyConversation2
     ]
  ]
        
-- | There shall be no data available once the echo program has
-- started. The listen command shall block
blockIfNoData1 :: Assertion
blockIfNoData1 = do
  d <- open echoProgram []
  v <- timeout shortTimeout $ listen d
  assertEqual "Shall be nothing" v Nothing
  
-- | After one tell/listen cycle to the echo program a new listen
-- command shall block due to no data
blockIfNoData2 :: Assertion
blockIfNoData2 = do
  d <- open echoProgram []
  let s = echoString 0
  tell d s
  _ <- listen d
  v <- timeout shortTimeout $ listen d
  assertEqual "Shall be nothing" v Nothing
  
-- | There shall be no data available once the echo program has
-- started. The listen all command shall block
blockIfNoData3 :: Assertion
blockIfNoData3 = do
  d <- open echoProgram []
  v <- timeout shortTimeout $ listenAll d
  assertEqual "Shall be nothing" v Nothing
  
-- | A listen following a tell shall echo the same data as told
asEchoIsTold :: Assertion
asEchoIsTold = do
  d <- open echoProgram []
  let s = echoString 0
  tell d s
  s' <- listen d
  assertEqual "Shall be equal" s s'
  
-- | A listen all following a tell shall echo the same data as told  
asEchoIsTold2 :: Assertion
asEchoIsTold2 = do
  d <- open echoProgram []
  let s = echoString 0
  tell d s
  s' <- listenAll d
  assertEqual "Shall be equal" [s] s'
  
-- | Follow a longer tell/listen conversation
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
  
-- | Follow a longer bursty tell * N -> listen * N conversation
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
  
-- | Follow a longer bursty tell * N -> listenAll conversation
followBurstyConversation2 :: Assertion
followBurstyConversation2 = do
  d <- open echoProgram []
  let ss = map echoString [0..10]
  mapM_ (tell d) ss
  -- This is a quite ugly hack to let the external process get all
  -- stuff on stout before reading
  threadDelay shortTimeout
  ss' <- listenAll d
  assertEqual "Shall be equal" ss ss'
  
echoProgram :: String
echoProgram = "./dist/build/Echo/Echo"

shortTimeout :: Int
shortTimeout = 1000 * 100 -- 100 ms expressed as microseconds

echoString :: Int -> BS.ByteString
echoString n = BS.pack $ "A string of text #" ++ show n