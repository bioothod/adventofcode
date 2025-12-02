{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Day21 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as X

import Debug.Trace (trace)
import Data.Maybe (mapMaybe, isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.MemoTrie

import Linear.V2

import Common.Search (aStar)
import Common.Utils1 (asciiGridMap, fullNeighboursOrt)

import qualified Data.Tuple
import Data.List (foldl', intercalate)

type Point = V2 Int

filterStar :: Char -> Maybe Char
filterStar x = if x == '*' then Nothing else Just x

type Pad = Map Point Char
type RevPad = Map Char Point


numPad :: Pad
numPad = asciiGridMap filterStar "789\n456\n123\n*0A"
dirPad :: Pad
dirPad = asciiGridMap filterStar "*^A\n<v>"

makeRevPad :: Ord k => Map a k -> Map k a
makeRevPad rpad = M.fromList $ map Data.Tuple.swap $ M.toList rpad

revNumPad :: RevPad
revNumPad = makeRevPad numPad
revDirPad :: RevPad
revDirPad = makeRevPad dirPad

selectPad :: (Eq a, Num a) => a -> Pad
selectPad i = if i == 0
              then numPad
              else dirPad
selectRevPad :: (Eq a, Num a) => a -> RevPad
selectRevPad i = if i == 0
                 then revNumPad
                 else revDirPad

lookupRevPad :: Int -> Char -> Point
lookupRevPad i v = case M.lookup v (selectRevPad i) of
                        Nothing -> error ("could not find " ++ show v ++ " in the rev pad")
                        Just x -> x

findNeighbours :: Pad -> Point -> [Point]
findNeighbours pad' p =
  let neigh = fullNeighboursOrt p
  in mapMaybe (\n -> if isNothing (M.lookup n pad') then Nothing else Just n) neigh

selectShortest :: [[Point]] -> [Point] -> Set (Point, Int) -> (Point, Int) -> Point -> [[Point]]
selectShortest acc current states start@(src, sdist) dst =
  let nextDist = sdist + 1
      neigh = fullNeighboursOrt src
      nextSteps = filter (\n -> S.member (n, nextDist) states) neigh
      current' = src : current
      nextTrees = foldl' (\acc' n -> selectShortest acc' current' states (n, nextDist) dst) acc nextSteps
  in if | src == dst -> seq current' $ reverse current' : acc
        | start `S.member` states -> nextTrees
        | otherwise -> acc

findShortestPaths :: Int -> Char -> Char -> [String]
findShortestPaths r startC finishC =
  let finishP = lookupRevPad r finishC
      startP = lookupRevPad r startC
      pad = selectPad r
      findN p = [(x, 1) | x <- findNeighbours pad p]

      states = S.fromList $ aStar findN (const 0) startP
      shortest = selectShortest [] [] states (startP, 0) finishP

      smapped = map (\path -> mapDirection path ++ ['A']) shortest
  in smapped


mapDirection :: [Point] -> String
mapDirection xs = zipWith dmap xs (drop 1 xs)
 where dmap a b = case b - a of
                    (V2 (-1) 0) -> '<'
                    (V2 1 0) -> '>'
                    (V2 0 (-1)) -> '^'
                    (V2 0 1) -> 'v'
                    _ -> error ("invalid diff: " ++ show a ++ " - " ++ show b)

walkPathPairs :: Int -> String -> [String]
walkPathPairs rid points =
  let pairs = zip ('A':points) points
      paths = reverse $ foldl' (\pathsAcc (s, e) -> let p = memo3 findShortestPaths rid s e
                                                    in p:pathsAcc
                     ) [] pairs
      all_paths = map concat $ sequence paths
--  in trace("walkPathPairs: rid: " ++ show rid ++ ", points: " ++ show points ++ ", paths:\n" ++ intercalate "\n" all_paths) all_paths
  in all_paths

-- walkFullPath :: Int -> [String] -> [[String]]
-- --walkFullPath rid chunks | trace("walkFullPath: rid: " ++ show rid ++ ", chunks: " ++ show chunks) False = undefined
-- walkFullPath rid chunks =
--   let chunk_paths = map (memo2 walkPathPairs rid) chunks
--       pathsExpanded = map concat chunk_paths
--   --in trace("walkFullPaths: rid: " ++ show rid ++ ", pathsExpanded:\n" ++ intercalate "\n" (map show pathsExpanded)) pathsExpanded
--   in pathsExpanded

iterateRobots :: [String] -> [Int] -> [String] -> [String]
--iterateRobots pathsAcc [] points | trace("pathsAcc: " ++ show pathsAcc ++ ", points: " ++ show points) False = undefined
iterateRobots pathsAcc [] points =
  let slen = length (head pathsAcc)
      plen = length (head points)
      acc = if | null pathsAcc -> points
               | plen == slen -> points ++ pathsAcc
               | plen < slen -> points
               | otherwise -> pathsAcc
  --in points ++ pathsAcc
  in acc

iterateRobots pathsAcc (r:rest) points =
  let paths = map (memo2 walkPathPairs r) points
      lens = map (sum . map length) paths
      min_len = minimum lens
      min_paths = mapMaybe (\(l, p) -> if l == min_len then Just p else Nothing) $ zip lens paths
      --paths' = trace("rest: " ++ show (length rest) ++ ", points: " ++ show (length points) ++ " : " ++ show (head points) ++ ", paths:\n" ++ intercalate "\n" (map show paths)) paths
      paths' = trace("rest: " ++ show (length rest) ++ ", points: " ++ show (length points) ++ " : " ++ show (head points) ++ ", paths:\n" ++ show (head min_paths)) min_paths

  in iterateRobots pathsAcc rest paths'

findScore :: [Int] -> String -> Int
findScore robots points =
  let allPaths = iterateRobots [] robots $ [[points]]
      --len = minimum $ map (\chunks -> sum $ map length chunks) (trace("allPaths: " ++ show (head allPaths)) allPaths)
      len = sum $ map length $ head allPaths
      num = read $ take 3 points
  --in trace(points ++ " len: " ++ show len ++ ", num: " ++ show num ++ ", allPaths:\n" ++ intercalate "\n" (map show allPaths)) (cache', num * len)
  in trace(points ++ " len: " ++ show len ++ ", num: " ++ show num ++ ", allPaths: " ++ show (length allPaths)) num * len

iterateCodes :: [Int] -> [String] -> Int
iterateCodes robots codes =
  sum $ map (findScore robots) codes

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do


  --let robots = zipWith makeRobot [0..] (numPad : replicate 1 dirPad)
  let codes = X.unpack input
  --let codes = "029A\n980A\n179A\n456A\n379A"
  let codes = "029A"
  iterateCodes [0..2] (lines codes)

solve2 :: ByteString -> Solution
solve2 input = do
  let codes = X.unpack input
  iterateCodes [0..25] (lines codes)
