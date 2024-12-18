module Day11 (solve1, solve2) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (foldl')

type Solution = Int

readInt :: String -> Int
readInt = read

type StepsLeft = Int
type Stone = Int
type StepsMap = Map StepsLeft Int
type Cache = Map Stone StepsMap

changeStone :: Stone -> [Stone]
changeStone s
  | s == 0 = [1]
  | even (numDigits :: Integer) = let divisor = 10 ^ (numDigits `div` 2)
                                      left = s `div` divisor
                                      right = s `mod` divisor
                                  in [left, right]
  | otherwise = [s * 2024]
 where numDigits = (+) 1 . floor . logBase 10 $ (fromIntegral s :: Double)

partialLookup :: Cache -> StepsLeft -> Stone -> Maybe Int
partialLookup _ 0 _ = Nothing
partialLookup cache n stone =
  case Map.lookup stone cache of
    Just m -> Map.lookup n m
    Nothing -> Nothing

runOneStep :: Cache -> StepsLeft -> [Stone] -> (Cache, Int)
runOneStep cache n = foldl' (\(cache', accLen) st -> let (nc, len) = convertStone cache' n st
                                                  in (nc, accLen+len))
                     (cache, 0)

updateCache :: Cache -> Stone -> StepsLeft -> Int -> Cache
updateCache cache stone n pathLen = let stepsCache = case Map.lookup stone cache of
                                          Just stepsCache' -> Map.insert n pathLen stepsCache'
                                          Nothing -> Map.fromList [(n, pathLen)]
                                 in Map.insert stone stepsCache cache

convertStone :: Cache -> StepsLeft -> Stone -> (Cache, Int)
convertStone cache 0 _stone = (cache, 1)
convertStone cache n stone =
  case partialLookup cache n stone of
    Just pathLen -> (cache, pathLen)
    Nothing -> let newStones = changeStone stone
                   (newCache, pathLen) = runOneStep cache (n-1) newStones
               in (updateCache newCache stone n pathLen, pathLen)

run :: Int -> String -> Solution
run n input = do
  let stones = map readInt $ words input
  let (_finalCache, totalLength) = foldl' (\(cache, accLen) st -> let (nc, len) = convertStone cache n st
                                                                  in (nc, accLen+len)) (Map.empty, 0) stones

  totalLength

solve1 :: String -> Solution
solve1 input = do
  run 25 input

solve2 :: String -> Solution
solve2 input = do
  run 75 input
