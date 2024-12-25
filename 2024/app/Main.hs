module Main (main) where

import Day22

import qualified Data.ByteString as BS (readFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
