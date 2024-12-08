module Day08 (solve1, solve2) where

import Data.Char (isAlphaNum, isAsciiUpper)
import Data.Maybe (fromJust, fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

type Solution = Int

type Freq = Char
data Coord = Coord Int Int
  deriving (Show, Eq, Ord)

data Node = Antenna Coord Freq
          | Anti Coord
  deriving (Show, Eq, Ord)

coords :: Node -> Coord
coords (Antenna c _) = c
coords (Anti c) = c

frequency :: Node -> Freq
frequency (Antenna _ f) = f
frequency (Anti _) = error "antinode does not have a frequency"

newNode :: Char -> Int -> Int -> Maybe Node
newNode c x y
  | isAlphaNum c || isAsciiUpper c = Just (Antenna (Coord x y) c)
  | otherwise = Nothing

parseLine :: Int -> [Char] -> [Node]
parseLine y rowStr = map fromJust $ filter (/= Nothing) $ zipWith (\c x -> newNode c x y) rowStr [1..]

type NodeMap = Map Coord Node
type FreqMap = Map Freq NodeMap

singleNodeMap :: FreqMap -> Node -> FreqMap
singleNodeMap freqM node = do
  let nodeMap = Map.lookup (frequency node) freqM
  let freqNode = fromMaybe Map.empty nodeMap

  let updatedNode = Map.insert (coords node) node freqNode
  Map.insert (frequency node) updatedNode freqM

mapLine :: FreqMap -> Int -> [Char] -> FreqMap
mapLine freqMap y rowStr = foldl (\fm node -> singleNodeMap fm node) freqMap $ parseLine y rowStr

isInside :: Int -> Int -> Coord -> Bool
isInside maxX maxY (Coord x y) = x >= 1 && x <= maxX && y >= 1 && y <= maxY

lineEquation :: Int -> Coord -> Coord -> Coord
lineEquation n (Coord x0 y0) (Coord x1 y1) = Coord x y
  where x = x0 - (x1 - x0) * n
        y = y0 - (y1 - y0) * n

type FindType = Int -> Int -> Coord -> [Coord] -> [Coord]

lineEquationFull :: Int -> Int -> Coord -> Coord -> [Coord]
lineEquationFull maxX maxY c0 c1 = a ++ b
 where a = takeWhile (isInside maxX maxY) $ map (\n -> lineEquation n c0 c1) [0..]
       b = takeWhile (isInside maxX maxY) $ map (\n -> lineEquation n c0 c1) [-1,-2..]

findAntinodesOne :: FindType
findAntinodesOne _ _ nodeCoord allCoords = map (lineEquation 1 nodeCoord) $ filter (/= nodeCoord) allCoords

findAntinodesLine :: FindType
findAntinodesLine maxX maxY nodeCoord allCoords = concat $ map (lineEquationFull maxX maxY nodeCoord) $ filter (/= nodeCoord) allCoords

findAntinodes :: FindType -> Int -> Int -> NodeMap -> [Coord]
findAntinodes findFunc maxX maxY nodeMap = do
  let allCoords = map fst $ Map.toList nodeMap
  concat $ map (\c -> findFunc maxX maxY c allCoords) allCoords

solve1 :: String -> Solution
solve1 input = do
  let allLines = lines input
  let maxX = length (head allLines)
  let maxY = length allLines
  let freqMap = foldl (\fm (y, row) -> mapLine fm y row) Map.empty $ zip [1..] allLines
  Set.size $ Set.fromList $ filter (isInside maxX maxY) $ concat $ map (findAntinodes findAntinodesOne maxX maxY . snd) $ Map.toList freqMap

solve2 :: String -> Solution
solve2 input = do
  let allLines = lines input
  let maxX = length (head allLines)
  let maxY = length allLines
  let freqMap = foldl (\fm (y, row) -> mapLine fm y row) Map.empty $ zip [1..] allLines

  let a = concatMap (findAntinodes findAntinodesOne maxX maxY . snd) $ Map.toList freqMap
      b = concatMap (findAntinodes findAntinodesLine maxX maxY . snd) $ Map.toList freqMap
    in Set.size $ Set.fromList $ filter (isInside maxX maxY) $ a ++ b
