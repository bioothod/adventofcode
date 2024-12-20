{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}



module Day20 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CX

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Foldable (foldl', find)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Either

import Common.Utils (Coord(..), genNextStepsCheckWeight, possibleCoords)
import Common.Search (aStar)
import Debug.Trace (trace)

charAllowed :: Char -> String -> Bool
charAllowed c s = c `elem` s

convertRow y row = map (\(x, c) -> (Coord x y, c)) $  zip [0..] $ CX.unpack row


selectCoords :: Int -> ByteString -> [(Coord, Char)]
selectCoords y row = mapMaybe (\(x, c) -> if charAllowed c "SE." then Just (Coord x y, c) else Nothing) $  zip [0..] $ CX.unpack row

aStarHeuristic :: Coord -> Coord -> Int
--aStarHeuristic (Coord fx fy) (Coord x y) = abs (fx - x) + abs (fy - y)
aStarHeuristic _ _ = 0

searchFinish :: (Coord -> Bool) -> Coord -> Coord -> [(Coord, Int)]
searchFinish checkFunction start finish = aStar (genNextStepsCheckWeight checkFunction (, 1)) (aStarHeuristic finish) start

checkCoordsSet :: Ord a => Set a -> a -> Bool
checkCoordsSet coords c = S.member c coords


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
  in if | pos == finish -> acc
        | null pairs -> []
        | otherwise -> let (selected, _cost) = minimumBy (compare `on` snd) pairs
                       in shortestPath allPaths selected finish (selected:acc)


checkPair :: Map Coord Int -> Coord -> Coord -> Maybe Int
checkPair coords a b = case (M.lookup a coords, M.lookup b coords) of
                         (Nothing, _) -> Nothing
                         (_, Nothing) -> Nothing
                         (Just p0, Just p1) -> Just (abs (p0 - p1) - 2)

isCheat :: Map Coord Int -> Coord -> Maybe Int
isCheat coords (Coord x y) = let horD = checkPair coords (Coord (x-1) y) (Coord (x+1) y)
                                 verD = checkPair coords (Coord x (y-1)) (Coord x (y+1))
                             in case (horD, verD) of
                                  (Nothing, d) -> d
                                  (d, Nothing) -> d
                                  (Just a, Just b) -> Just (max a b)

findCheats :: [Coord] -> Map Coord Int -> [(Coord, Int)]
findCheats walls coords = mapMaybe (\coord -> case isCheat coords coord of
                                                Nothing -> Nothing
                                                Just d -> Just (coord, d)
                                   ) walls

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do
  let cChars = concat $ zipWith convertRow [0..] $ CX.lines input
  let start = fst $ fromMaybe (error "no start") $ find ((=='S') . snd) cChars
  let finish = fst $ fromMaybe (error "no finish") $ find ((=='E') . snd) cChars
  let (coords, walls) = foldl (\(accCoord, accWalls) (coord, c) -> if c == '#'
                                                                   then (accCoord, coord:accWalls)
                                                                   else (S.insert coord accCoord, accWalls)
                              ) (S.empty, []) cChars

  let pathsFS = M.fromList $ searchFinish (checkCoordsSet coords) finish start
  let pathsSF = M.fromList $ searchFinish (checkCoordsSet coords) start finish
  let shortestPathLength = fromMaybe (error "paths map does not contains finish") (M.lookup finish pathsSF)
  let cheats = findCheats walls pathsSF
  let sel = filter (\(_, sp) -> sp >= 100) cheats
  length sel

solve2 :: ByteString -> Solution
solve2 input = do
  2
