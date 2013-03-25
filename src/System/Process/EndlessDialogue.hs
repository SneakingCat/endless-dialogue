module System.Process.EndlessDialogue (
  Dialogue
  , open
  , tell
  , listen
  ) where

import System.Process (ProcessHandle, waitForProcess, runInteractiveProcess)
import System.Exit (ExitCode(..))
import System.IO (Handle, hSetBinaryMode, hWaitForInput, hFlush)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8 as BS

type BSMVar = MVar BS.ByteString
data Dialogue = Dialogue (Handle,Handle,Handle,ProcessHandle) BSMVar BSMVar

-- | Open a dialogue to the specified program
open :: String -> [String] -> IO Dialogue
open cmd args = do
  -- Create the external process
  h@(hIn,hOut,_,_) <- runInteractiveProcess cmd args Nothing Nothing
  
  -- Modify the file handles to operate in text mode
  hSetBinaryMode hIn False
  hSetBinaryMode hOut False
  
  -- Create MVars to communicate with the IO managing threads
  vIn  <- newEmptyMVar
  vOut <- newEmptyMVar
  
  -- Create the dialogue instance
  let dialogue = Dialogue h vIn vOut
  
  -- Supervise the external process
  _ <- forkIO $ supervise dialogue
  
  -- Threads to communicate with the external process
  _ <- forkIO $ tellT dialogue
  _ <- forkIO $ listenT dialogue
  
  -- Return the dialogue to the user
  return dialogue

-- | Tell the dialogue a line of text
tell :: Dialogue -> BS.ByteString -> IO ()
tell (Dialogue _ vIn _) = putMVar vIn

-- | Listen to one line of text. The listen is blocking
listen :: Dialogue -> IO BS.ByteString
listen (Dialogue _ _ vOut) = takeMVar vOut

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
tellT d@(Dialogue (hIn,_,_,_) vIn _) = do
  s <- takeMVar vIn
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
listenT' d@(Dialogue (_,hOut,_,_) _ vOut) = do
  s <- BS.hGetLine hOut
  putMVar vOut s
  listenT' d