module Day06 (solve1, solve2) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sortBy)
import Data.Set (Set)
import qualified Data.Set as Set

type Solution = Int

data Coord = Coord Int Int
  deriving (Show, Eq, Ord)

data Direction = UpD
               | DownD
               | LeftD
               | RightD
               deriving (Show, Eq, Ord)

data Point = Guard Coord Direction
           | Obstacle Coord
           | Exit Coord
           | Empty Coord
           deriving (Show, Eq, Ord)

isExit :: Point -> Bool
isExit (Exit _) = True
isExit _ = False

isEmpty :: Point -> Bool
isEmpty (Empty _) = True
isEmpty _ = False

coordX :: Point -> Int
coordX (Obstacle (Coord x _)) = x
coordX (Guard (Coord x _) _) = x
coordX (Exit (Coord x _)) = x
coordX (Empty (Coord x _)) = x

coordY :: Point -> Int
coordY (Obstacle (Coord _ y)) = y
coordY (Guard (Coord _ y) _) = y
coordY (Exit (Coord _ y)) = y
coordY (Empty (Coord _ y)) = y

direction :: Point -> Direction
direction (Guard _ d) = d
direction _ = error "can not get direction of this object"

coordinates :: Point -> Coord
coordinates (Guard c _) = c
coordinates (Obstacle c) = c
coordinates (Exit c) = c
coordinates (Empty c) = c

type MapType = Map Int [Point]
-- GuardMapN means it is indexed by N coordinate
type GuardMapY = MapType
type GuardMapX = MapType

parsePoint :: Char -> Int -> Int -> Point
parsePoint c x y
  | c == '.' = Empty (Coord x y)
  | c == '#' = Obstacle (Coord x y)
  | c == '^' = Guard (Coord x y) UpD
  | otherwise = error ("unsupported point found: " ++ (show c) ++ " at x:" ++ (show x) ++ ", y:" ++ (show y))

mergeKeys :: p -> [a] -> [a] -> [a]
mergeKeys _ new_value old_value = old_value ++ new_value
coordMapInsert :: Int -> Point -> MapType -> MapType
coordMapInsert k v m = Map.insertWithKey mergeKeys k [v] m

updateMaps :: (GuardMapX, GuardMapY, Point) -> Point -> (GuardMapX, GuardMapY, Point)
updateMaps (mx, my, g) p = case p of
                             Guard _ _ -> (mx, my, p)
                             Obstacle (Coord x y) -> (coordMapInsert x p mx, coordMapInsert y p my, g)
                             _ -> error "can not have an empty point"

sortFunction :: Point -> Point -> Ordering
sortFunction a b
  | ax == bx = compare ay by
  | ay == by = compare ax bx
  | otherwise = error ("comparing unsupported points: a: " ++ (show a) ++ ", b: " ++ (show b))
 where Coord ax ay = coordinates a
       Coord bx by = coordinates b

sortMapKeys :: MapType -> MapType
sortMapKeys m = foldl sortInsert Map.empty $ Map.toList m
  where sortInsert accmap (k, vl) = Map.insert k (sortBy sortFunction vl) accmap

parseInput :: Int -> String -> [Point]
parseInput y row = zipWith (\x c -> parsePoint c x y) [1..] row

coordMapInsertGuard :: MapType -> [Int] -> [Point] -> MapType
coordMapInsertGuard m keys guards = foldl (\macc (key, g) -> coordMapInsert key g macc) m $ zip keys guards

guardRun :: Set Coord -> Set Point -> GuardMapX -> GuardMapY -> Point -> (Set Coord, Bool)
guardRun uniqPos fullPath mx my g = do
  let dir = direction g
  let new_dir = case dir of
                   UpD -> RightD
                   RightD -> DownD
                   DownD -> LeftD
                   LeftD -> UpD

  let (gx, gy) = (coordX g, coordY g)

  let (key, m) = case dir of
        UpD -> (gx, mx)
        DownD -> (gx, mx)
        LeftD -> (gy, my)
        RightD -> (gy, my)

  let observations = case Map.lookup key m of
        Nothing -> error "impossible, we have guards"
        Just l -> l

  let obs = head $ case dir of
                     UpD -> dropWhile (\o -> (coordY o) > gy) (reverse observations)
                     DownD -> dropWhile (\o -> (coordY o) < gy) observations
                     LeftD -> dropWhile (\o -> (coordX o) > gx) (reverse observations)
                     RightD -> dropWhile (\o -> (coordX o) < gx) observations

  let obsX = coordX obs
  let obsY = coordY obs

  let new_guard = case dir of
        UpD -> Guard (Coord obsX (obsY+1)) new_dir
        DownD -> Guard (Coord obsX (obsY-1)) new_dir
        LeftD -> Guard (Coord (obsX+1) obsY) new_dir
        RightD -> Guard (Coord (obsX-1) obsY) new_dir

  let coords = case dir of
        UpD -> map (\k -> Coord obsX k) [(coordY new_guard)..gy]
        DownD -> map (\k -> Coord obsX k) [gy..(coordY new_guard)]
        LeftD -> map (\k -> Coord k obsY) [(coordX new_guard)..gx]
        RightD -> map (\k -> Coord k obsY) [gx..(coordX new_guard)]

  let upUniqPos = foldl (\acc k -> Set.insert k acc) uniqPos coords
  let upFullPath = foldl (\acc k -> Set.insert (Guard k dir) acc) fullPath coords
  let isCycle = Set.member new_guard fullPath

  if isExit obs || isCycle
    then (upUniqPos, isCycle)
    else guardRun upUniqPos upFullPath mx my new_guard



solve1 :: String -> Solution
solve1 input = do
  let char_map = lines input

  let maxGuardX = 1 + (length $ head char_map)
  let maxGuardY = 1 + length char_map

  let all_points = concat $ zipWith parseInput [1..] char_map
  let points = filter (not . isEmpty) all_points

  let (mx, my, g) = foldl updateMaps (Map.empty, Map.empty, Empty (Coord 0 0)) points

  let guardKeysX = [0..maxGuardX]
  let mx1 = coordMapInsertGuard mx guardKeysX (map (\x -> Exit (Coord x 0)) guardKeysX)
  let nmx = sortMapKeys $ coordMapInsertGuard mx1 guardKeysX (map (\x -> Exit (Coord x maxGuardY)) guardKeysX)


  let guardKeysY = [0..maxGuardY]
  let my1 = coordMapInsertGuard my guardKeysY (map (\y -> Exit (Coord 0 y)) guardKeysY)
  let nmy = sortMapKeys $ coordMapInsertGuard my1 guardKeysY (map (\y -> Exit (Coord maxGuardX y)) guardKeysY)

  let (s, _) = guardRun Set.empty Set.empty nmx nmy g
  Set.size s

oneRunCycle :: Int -> Int -> Point -> [Point] -> Bool
oneRunCycle maxGuardX maxGuardY obs points = do
  -- could be optimized to only insert points into already filled maps
  let new_points = obs:points
  let (mx, my, g) = foldl updateMaps (Map.empty, Map.empty, Empty (Coord 0 0)) new_points

  let guardKeysX = [0..maxGuardX]
  let mx1 = coordMapInsertGuard mx guardKeysX (map (\x -> Exit (Coord x 0)) guardKeysX)
  let nmx = sortMapKeys $ coordMapInsertGuard mx1 guardKeysX (map (\x -> Exit (Coord x maxGuardY)) guardKeysX)

  let guardKeysY = [0..maxGuardY]
  let my1 = coordMapInsertGuard my guardKeysY (map (\y -> Exit (Coord 0 y)) guardKeysY)
  let nmy = sortMapKeys $ coordMapInsertGuard my1 guardKeysY (map (\y -> Exit (Coord maxGuardX y)) guardKeysY)

  let (_, isCycle) = guardRun Set.empty Set.empty nmx nmy g
  isCycle

solve2 :: String -> Solution
solve2 input = do
  let char_map = lines input

  let maxGuardX = 1 + (length $ head char_map)
  let maxGuardY = 1 + length char_map

  let all_points = concat $ zipWith parseInput [1..] char_map
  let points = filter (not . isEmpty) all_points
  let empty = filter isEmpty all_points

  length . filter (==True) $ map (\p -> oneRunCycle maxGuardX maxGuardY (Obstacle (coordinates p)) points) empty
