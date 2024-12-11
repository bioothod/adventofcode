module Day10 (solve1, solve2) where

import Data.Char (digitToInt)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.Set as Set

import Data.List (sort, group)

type Solution = Int

parseString :: String -> [Int]
parseString = map digitToInt

findStart :: Int -> [Int] -> [(Int, Int)]
findStart y row = map (\(_, x) -> (x, y)) $ filter ((==0) . fst) $ zip row [0..]

checkCoord :: Vector (Vector Int) -> (Int, Int) -> Bool
checkCoord mt (x, y) = (x >= 0) && (x<Vector.length (mt Vector.! 0)) && (y>=0) && (y<Vector.length mt)

possibleFromCoord :: Vector (Vector Int) -> (Int, Int) -> [(Int, Int)]
possibleFromCoord mt (x, y) = filter (checkCoord mt) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

getM :: Vector (Vector Int) -> Int -> Int -> Int
getM mt x y = let row = mt Vector.! y
                     in row Vector.! x

getAndMatch :: Vector (Vector Int) -> Int -> (Int, Int) -> Bool
getAndMatch mt value (x, y) = (getM mt x y) == (value+1)

walkTrail :: Vector (Vector Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
walkTrail mt acc (x, y) = do
  let value = getM mt x y
  let possibleCoords = possibleFromCoord mt (x, y)
  let goodCoords = filter (getAndMatch mt value) possibleCoords
  case value of
    9 -> (x, y):acc
    _ -> foldl (walkTrail mt) acc goodCoords

solve1 :: String -> Solution
solve1 input = do
  let inps = map parseString $ lines input

  let starts = concat $ filter (not.null) $ zipWith findStart [0..] inps
  let mt = Vector.fromList $ map Vector.fromList inps

  let found = map (Set.fromList . walkTrail mt []) starts
  let res = map length found

  sum res

calcScore :: [(Int, Int)] -> Int
calcScore = sum . map length . group . sort

solve2 :: String -> Solution
solve2 input = do
  let inps = map parseString $ lines input

  let starts = concat $ filter (not.null) $ zipWith findStart [0..] inps
  let mt = Vector.fromList $ map Vector.fromList inps

  sum $ map (calcScore . walkTrail mt []) starts
