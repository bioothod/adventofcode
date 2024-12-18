module Day18 (solve1, solve2) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Common.Utils (Coord(..), wordsWhen)
import Common.Search (aStar)
import Debug.Trace (trace)
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)

parseCoords :: String -> Coord
parseCoords row = let ([x], [y]) = splitAt 1 $ map read $ wordsWhen (==',') row
                  in Coord x y

type CorruptedBlocks = Set Coord
type Limits = (Int, Int)

possibleCoords :: Coord -> [Coord]
possibleCoords (Coord x y) = [Coord (x+1) y, Coord (x-1) y, Coord x (y+1), Coord x (y-1)]

genNextSteps :: CorruptedBlocks -> Limits -> Coord -> [(Coord, Int)]
genNextSteps corrupted (maxX, maxY) pos =
  map (\c -> (c, 1)) $
  filter (\c -> not $ S.member c corrupted) $
  filter (\(Coord cx cy) -> cx >= 0 && cx <= maxX && cy >= 0 && cy <= maxY) $
  possibleCoords pos


aStarHeuristic :: Coord -> Coord -> Int
--aStarHeuristic (Coord fx fy) (Coord x y) = abs (fx - x) + abs (fy - y)
aStarHeuristic _ _ = 0

searchFinish :: CorruptedBlocks -> Limits -> Coord -> Coord -> [(Coord, Int)]
searchFinish corrupted limits start finish = aStar (genNextSteps corrupted limits) (aStarHeuristic finish) start

findMaybes :: Map Coord Int -> [Coord] -> [(Coord, Int)]
findMaybes allPaths possible =
  let pairs = mapMaybe (\coord -> case M.lookup coord allPaths of
                                    Nothing -> Nothing
                                    Just cost -> Just (coord, cost)
                       ) possible
  in pairs

shortestPath :: Map Coord Int -> Coord -> Coord -> [Coord] -> [Coord]
shortestPath allPaths pos finish acc =
  let posC = possibleCoords pos
      pairs = findMaybes allPaths posC
  in if pos == finish
     then acc
     else
        if null pairs
          then []
          else let (selected, _cost) = minimumBy (compare `on` snd) pairs
               in shortestPath allPaths selected finish (selected:acc)

type Solution = Int
solve1 :: String -> Solution
solve1 input = do
  let blocks = map parseCoords $ lines input
  let (maxX, maxY) = (70, 70)
  let start = Coord 0 0
  let finish = Coord maxX maxY

  let corrupted = S.fromList $ take 1024 blocks

  let allPaths = searchFinish corrupted (maxX, maxY) start finish
  let path = shortestPath (M.fromList allPaths) finish start []

  length path

findNewPath :: Limits -> CorruptedBlocks -> Coord -> Coord -> Set Coord
findNewPath lim corrupted start finish = do
  let allPaths = searchFinish corrupted lim start finish
  let path = shortestPath (M.fromList allPaths) finish start []
  S.fromList path

updateAndSearch :: Limits -> CorruptedBlocks -> [Coord] -> Set Coord -> Coord -> Coord -> Coord
updateAndSearch _ _ [] _ _ _ = error "no more blocks"
updateAndSearch lim corruptedOld (block:restBlocks) currentPath start finish = do
  let corrupted = S.insert block corruptedOld

  let newPath = if (block `S.member` currentPath) || null currentPath
        then findNewPath lim corrupted start finish
        else currentPath

  if null newPath
    then block
    else updateAndSearch lim corrupted restBlocks newPath start finish

solve2 :: String -> Solution
solve2 input = do
  let blocks = map parseCoords $ lines input
  let lim@(maxX, maxY) = (70, 70)
  let numBlocks = 1024
  let start = Coord 0 0
  let finish = Coord maxX maxY

  let corrupted = S.fromList $ take numBlocks blocks
  let (Coord x y) = updateAndSearch lim corrupted (drop numBlocks blocks) S.empty start finish
  trace("block: " ++ show x ++ "," ++ show y) 1
