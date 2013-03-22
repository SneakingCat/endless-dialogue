module System.Process.EndlessDialogue (
  Dialogue
  , open
  , maybeListen
  ) where

import qualified Data.ByteString.Char8 as BS

data Dialogue = Dialogue

-- | Open a dialogue to the specified program
open :: String -> [String] -> IO Dialogue
open cmd args = return Dialogue

-- | Listen non-blocking to the dialogue
maybeListen :: Dialogue -> IO (Maybe BS.ByteString)
maybeListen d = return Nothing
