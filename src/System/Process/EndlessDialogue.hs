module System.Process.EndlessDialogue (
  Dialogue
  , open
  , tell
  , listen
  , listenAll
  ) where

import System.Process (ProcessHandle, waitForProcess, runInteractiveProcess)
import System.Exit (ExitCode(..))
import System.IO (Handle, hSetBinaryMode, hWaitForInput, hFlush)
import Control.Concurrent (Chan, forkIO, newChan, writeChan, 
                           readChan, isEmptyChan)
import qualified Data.ByteString.Char8 as BS

type BSChan = Chan BS.ByteString
data Dialogue = Dialogue (Handle,Handle,Handle,ProcessHandle) BSChan BSChan

-- | Open a dialogue to the specified program
open :: String -> [String] -> IO Dialogue
open cmd args = do
  -- Create the external process
  h@(hIn,hOut,_,_) <- runInteractiveProcess cmd args Nothing Nothing
  
  -- Modify the file handles to operate in text mode
  hSetBinaryMode hIn False
  hSetBinaryMode hOut False
  
  -- Create Chans to communicate with the IO managing threads
  cIn  <- newChan
  cOut <- newChan
  
  -- Create the dialogue instance
  let dialogue = Dialogue h cIn cOut
  
  -- Supervise the external process
  _ <- forkIO $ supervise dialogue
  
  -- Threads to communicate with the external process
  _ <- forkIO $ tellT dialogue
  _ <- forkIO $ listenT dialogue
  
  -- Return the dialogue to the user
  return dialogue

-- | Tell the dialogue a line of text
tell :: Dialogue -> BS.ByteString -> IO ()
tell (Dialogue _ cIn _) = writeChan cIn

-- | Listen to one line of text. The listen is blocking
listen :: Dialogue -> IO BS.ByteString
listen (Dialogue _ _ cOut) = readChan cOut

-- | Listen to all content that is available in the stdin buffer. The
-- first line will be read blocking, and the rest of the content will
-- be read non-blocking if available
listenAll :: Dialogue -> IO [BS.ByteString]
listenAll (Dialogue _ _ cOut) = do
  s  <- readChan cOut
  s' <- readChanIfAvailable cOut
  return (s:s')
  
readChanIfAvailable :: BSChan -> IO [BS.ByteString]
readChanIfAvailable cOut = do
  emptyChan <- isEmptyChan cOut
  case emptyChan of
    True  -> return []
    False -> do
      s  <- readChan cOut
      s' <- readChanIfAvailable cOut
      return (s:s')

-- | Supervise a started external program. Signal to the user with an
-- io error
supervise :: Dialogue -> IO ()
supervise (Dialogue (_,_,_,pid) _ _) = do
  code <- waitForProcess pid
  case code of
    ExitSuccess       ->
      ioError $ errorWithCode 0
    ExitFailure code' ->
      ioError $ errorWithCode code'
  where
    errorWithCode :: Int -> IOError
    errorWithCode c = userError $ "Process terminated with code: " ++ show c
                         
-- | Wait for a line of text from the user to become available. Copy
-- the content to the external process' stdin
tellT :: Dialogue -> IO ()
tellT d@(Dialogue (hIn,_,_,_) cIn _) = do
  s <- readChan cIn
  BS.hPutStrLn hIn s
  hFlush hIn
  tellT d
  
-- | Wait for a line of text from the external process to become
-- available. Copy the content to the user.
listenT :: Dialogue -> IO ()
listenT d@(Dialogue (_,hOut,_,_) _ _) = do
  _ <- hWaitForInput hOut (-1) -- Initial wait for the handle
  listenT' d
  
listenT' :: Dialogue -> IO ()
listenT' d@(Dialogue (_,hOut,_,_) _ cOut) = do
  s <- BS.hGetLine hOut
  writeChan cOut s
  listenT' d