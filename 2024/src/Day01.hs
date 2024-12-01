module Day01 (solve1, solve2) where

import Data.List (sort)
--import Text.ParserCombinators.Parsec

type Solution = Integer

distance :: (Integer, Integer) -> Integer
distance (a, b) = if a < b
                  then b - a
                  else a - b

convertLine :: String -> [Integer]
convertLine line = map read (words line)

split :: ([a], [a]) -> [[a]] -> ([a], [a])
split (acx, acy) [] = (acx, acy)
split (acx, acy) ([x,y]:rest) = split (x:acx, y:acy) rest

map2int :: [String] -> [[Integer]]
map2int ls = map convertLine ls

solve1 :: String -> Solution
solve1 input = do
  let ints = map2int (lines input)
  let (acx, acy) = split ([], []) ints
  sum (map distance (zip (sort acx) (sort acy)))

count_other :: Integer -> [Integer] -> Integer
count_other l rs = toInteger . length $ filter (==l) rs

solve2 :: String -> Solution
solve2 input = do
  let ints = map2int (lines input)
  let (acx, acy) = split ([], []) ints
  let scores = [l * count_other l acy | l <- acx]
  sum scores
