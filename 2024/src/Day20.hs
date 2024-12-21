{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}



module Day20 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CX

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Foldable (foldl', find)
import Data.Function (on)
import Data.List (minimumBy)

import Common.Utils (Coord(..), genNextStepsCheckWeight)
import Common.Search (aStar)

convertRow :: Int -> ByteString -> [(Coord, Char)]
convertRow y row = map (\(x, c) -> (Coord x y, c)) $  zip [0..] $ CX.unpack row

aStarHeuristic :: Coord -> Coord -> Int
aStarHeuristic _ _ = 0

searchFinish :: (Coord -> Bool) -> Coord -> Coord -> [(Coord, Int)]
searchFinish checkFunction start finish = aStar (genNextStepsCheckWeight checkFunction (, 1)) (aStarHeuristic finish) start

checkCoordsSet :: Ord a => Set a -> a -> Bool
checkCoordsSet coords c = S.member c coords

calcDist :: Map Coord Int -> Coord -> Coord -> Maybe Int
calcDist coords a b = case (M.lookup a coords, M.lookup b coords) of
                        (Nothing, _) -> Nothing
                        (_, Nothing) -> Nothing
                        (Just p0, Just p1) -> Just (abs (p0 - p1))

calcMhDist :: Coord -> Coord -> Int
calcMhDist (Coord ax ay) (Coord bx by) = abs (ax - bx) + abs (ay - by)

checkPair :: Map Coord Int -> Coord -> Coord -> Maybe Int
checkPair coords a b = let mhDist = calcMhDist a b
                           dist = calcDist coords a b
                       in case dist of
                            Nothing -> Nothing
                            Just d -> Just (d - mhDist)

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

isCheatDist :: Map Coord Int -> Int -> Coord -> Coord -> Bool
isCheatDist allPaths minSave a b = case checkPair allPaths a b of
                                     Nothing -> False
                                     Just d -> d >= minSave



findCheatsDistOne ::Map Coord Int -> Int -> (Set (Coord, Coord), Map Coord Int) -> Coord -> (Set (Coord, Coord), Map Coord Int)
findCheatsDistOne allPaths maxDist acc pos@(Coord cx cy) = do
  let othersOnPath = filter (\c -> calcMhDist c pos <= maxDist && M.member c allPaths) [Coord (cx+x) (cy+y) | x <- [-maxDist..maxDist], y <- [-maxDist..maxDist]]
  foldl' (\(accChecked, accToCheck) coord -> let pair = if coord < pos then (coord, pos) else (pos, coord)
                                                 accToCheck' = M.delete coord accToCheck
                                                 accChecked' = S.insert pair accChecked
                                             in (accChecked', accToCheck'))
    acc othersOnPath

findCheatsDist :: Map Coord Int -> Int -> Int -> Int
findCheatsDist allPaths maxDist minSave = do
  let (accChecked, _) = foldl' (findCheatsDistOne allPaths maxDist) (S.empty, allPaths) (M.keys allPaths)
  length $ filter (uncurry (isCheatDist allPaths minSave)) (S.toList accChecked)


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

  let pathsSF = M.fromList $ searchFinish (checkCoordsSet coords) start finish
  let cheats = findCheats walls pathsSF
  let sel = filter (\(_, sp) -> sp >= 100) cheats
  length sel

solve2 :: ByteString -> Solution
solve2 input = do
  let cChars = concat $ zipWith convertRow [0..] $ CX.lines input
  let start = fst $ fromMaybe (error "no start") $ find ((=='S') . snd) cChars
  let finish = fst $ fromMaybe (error "no finish") $ find ((=='E') . snd) cChars
  let (coords, _) = foldl (\(accCoord, accWalls) (coord, c) -> if c == '#'
                                                               then (accCoord, coord:accWalls)
                                                               else (S.insert coord accCoord, accWalls)
                          ) (S.empty, []) cChars

  let allPaths = M.fromList $ searchFinish (checkCoordsSet coords) start finish
  findCheatsDist allPaths 20 100
