module Main where

import System.IO

main :: IO ()
main = echo

echo :: IO ()
echo = do
  s <- hGetLine stdin
  hPutStrLn stdout s
  hFlush stdout
  echo