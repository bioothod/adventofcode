module Main (main) where

import Day14

import System.Environment (getArgs)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
