{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Day21 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as X

import Debug.Trace (trace)
import Data.Maybe (fromJust, catMaybes, isJust, mapMaybe, isNothing, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A

import Data.List (transpose, elemIndex, intercalate, foldl')

import Linear.V2
import Linear.V3

import Lens.Micro.Platform

import Common.Search (aStar)
import qualified Data.Tuple

type Point = V2 Int

-- | Get X
getX :: R1 t => t a -> a
getX = view _x

-- | Get Y
getY :: R2 t => t a -> a
getY = view _y

-- | Get Z
getZ :: R3 t => t a -> a
getZ = view _z

-- | Gets the Chebyshev neighbors of an N-dimensional point.
fullNeighbors :: (Applicative f, Traversable f, Num (f a), Num a, Eq (f a)) => f a -> [f a]
fullNeighbors pt = [pt + dir | dir <- sequenceA (pure [-1, 0, 1]), dir /= pure 0]

fullNeighboursOrt :: (Integral a) => V2 a -> [V2 a]
fullNeighboursOrt pt = [pt + step | step <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]


-- | Computes the Manhattan distance between two N-dimensional points.
manhattan :: (Foldable f, Num (f a), Num a) => f a -> f a -> a
manhattan pt1 pt2 = sum $ abs (pt1 - pt2)

-- CARDINAL DIRECTIONS
-- | Cardinal directions
data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

-- | Enumeration of the directions
allDirs :: [Dir]
allDirs = [North ..]

-- | Gets the basis vector for a given direction
-- North is (0,1) here
dirPoint :: Num a => Dir -> V2 a
dirPoint North = V2   0   1
dirPoint East  = V2   1   0
dirPoint West  = V2 (-1)  0
dirPoint South = V2   0 (-1)

-- | Gets the basis vector for a given direction
-- North is (0,-1) here
dirPoint' :: Num a => Dir -> V2 a
dirPoint' North = V2   0 (-1)
dirPoint' East  = V2   1   0
dirPoint' West  = V2 (-1)  0
dirPoint' South = V2   0   1

-- | Rotates a point using a given direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint North (V2 x y) = V2   x    y
rotPoint East  (V2 x y) = V2   y  (-x)
rotPoint West  (V2 x y) = V2 (-y)   x
rotPoint South (V2 x y) = V2 (-x) (-y)

-- | Returns the minimum corner of a grid
-- | Only works on non-empty grids
minCorner :: Set (f a) -> f a
minCorner grid = case S.lookupMin grid of
                   Nothing  -> error "minCorner: empty grid"
                   Just m -> m

-- | Returns the maximum corner of a grid
-- | Only works on non-empty grids
maxCorner :: Set (f a) -> f a
maxCorner grid = case S.lookupMax grid of
                   Nothing  -> error "maxCorner: empty grid"
                   Just m -> m


-- 2D GRIDS
-- | Creates an array representing a 2D grid from a String
-- Requires newlines between the rows of the grid
--asciiGridArray :: (Char -> a) -> String -> Array Point DstValue
asciiGridArray f str = A.listArray (0, V2 maxX maxY)
                     $ concat $ transpose rows
  where
    maxX = length (head rows) - 1
    maxY = length rows - 1
    rows = map (map f) $ lines str


rowToList :: Int -> [c] -> [(Point, c)]
rowToList y row = zip [(V2 x y) | x <- [0..]] row

-- | Creates a map representing a 2D grid from a String
-- Requires newlines between the rows of the grid
--asciiGridMap :: (Char -> Maybe DstValue) -> String -> Map Point DstValue
asciiGridMap f text = let clist = concatMap (uncurry $ rowToList) $ zip [0..] $ lines text
                          filtered = mapMaybe (\(c, l) -> if isNothing (f l) then Nothing else Just (c, l)) clist
                      in M.fromList filtered

-- | Displays a Map of Points as a String
displayAsciiMap
    :: Char             -- ^ missing char
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap missing grid = unlines
    [ [ M.findWithDefault missing (V2 x y) grid
      | x <- [xMin .. xMax]]
    | y <- [yMin .. yMax]]
  where
   set = M.keysSet grid
   (V2 xMin yMin) = minCorner set
   (V2 xMax yMax) = maxCorner set

-- | Displays a Set of Points as a String
displayAsciiSet
    :: Char      -- ^ missing char
    -> Char      -- ^ present char
    -> Set Point -- ^ tile set
    -> String
displayAsciiSet missing here =
  displayAsciiMap missing . M.fromSet (const here)

filterStar :: Char -> Maybe Char
filterStar x = if x == '*' then Nothing else Just x

data Cache = Cache {_p2 :: Map (Point, Point) [[Point]], _d2 :: Map (Char, Char) [String], _full :: Map String [[[Point]]]}
  deriving Show
makeLenses ''Cache
makeCache :: Cache
makeCache = Cache M.empty M.empty M.empty

type Pad = Map Point Char
type RevPad = Map Char Point

data Robot = Robot {_id :: Int, _pos :: Point, _pad :: Pad, _rev :: RevPad }
  deriving Show
makeLenses ''Robot

lookupPad :: Robot -> Point -> Char
lookupPad r v = case M.lookup v (r^.pad) of
  Nothing -> error ("could not find " ++ show v ++ " in the pad")
  Just x -> x

lookupRevPad :: Robot -> Char -> Point
lookupRevPad r v = case M.lookup v (r^.rev) of
  Nothing -> error ("could not find " ++ show v ++ " in the rev pad")
  Just x -> x

makeRobot :: Int -> Map Point Char -> Robot
makeRobot rid rpad =
  let apos = filter (\x -> snd x == 'A') (M.toList rpad)
      revp = M.fromList $ map Data.Tuple.swap $ M.toList rpad
  in if null apos
     then error "could not find A"
     else Robot rid (fst $ head apos) rpad revp

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


findShortestPaths :: Cache -> Robot -> Char -> (Cache, [[Point]])
findShortestPaths cache r finishC =
  let findN p = [(x, 1) | x <- findNeighbours (r^.pad) p]
      startP = r^.pos
      finishP = lookupRevPad r finishC
      states = S.fromList $ aStar findN (const 0) startP
      shortest = selectShortest [] [] states (startP, 0) finishP

      startC = lookupPad r startP
      smapped = map (\path -> mapDirection path ++ ['A']) shortest
      cache' = over p2 (M.insert (startP, finishP) shortest) cache
  in case M.lookup (startP, finishP) (view p2 cache) of
       Nothing -> trace("findShortestPaths: " ++ show (_id r) ++ ": " ++ show (startC, finishC) ++ " -> " ++ show smapped)(cache', shortest)
       Just s -> (cache, s)


mapDirection :: [Point] -> [Char]
mapDirection xs = zipWith dmap xs (drop 1 xs)
                       where dmap a b = case b - a of
                                          (V2 (-1) 0) -> '<'
                                          (V2 1 0) -> '>'
                                          (V2 0 (-1)) -> '^'
                                          (V2 0 1) -> 'v'
                                          _ -> error ("invalid diff: " ++ show a ++ " - " ++ show b)

--mapPath :: Robot -> [Point] -> [Char]
--mapPath r xs = map (lookupPad r) xs

walkByStep :: Cache -> [[[Point]]] -> [[Point]] -> Robot -> String -> (Cache, [[[Point]]])
walkByStep cache acc cur _r [] = (cache, reverse cur : acc)
--walkByStep acc cur r points | trace("walkByStep: start r: " ++ show (_id r) ++ ", cur: " ++ show (map mapDirection cur) ++ ", points: " ++ points ++ ", p2: " ++ show (M.size (r^.cache.p2)) ++ ", d2: " ++ show (M.size (r^.cache.d2)) ++ ", full: " ++ show (M.size (r^.cache.full))) False = undefined
walkByStep cache acc cur r points =
  let dstC = head points
      dstP = lookupRevPad r dstC
      (cache', paths) = findShortestPaths cache r dstC
      (cache''', allPaths) = foldl' (\(cache'', acc') path ->
                                   walkByStep cache'' acc' (path:cur) (set pos dstP r) (tail points))
                         (cache', acc) paths
  in (cache''', allPaths)

walkAllPaths :: Cache -> Robot -> [Char] -> (Cache, [[Char]])
walkAllPaths cache r points =
  let selectShortestPaths ps =
        let lengths = map length ps
            minLen = minimum lengths
        in mapMaybe (\(p, l) -> if l == minLen then Just p else Nothing) $ zip ps lengths

      (cache', paths) = walkByStep cache [] [] r points

      pathsDirections = map (map (\path -> mapDirection path ++ ['A'])) paths
      mappedDirs = selectShortestPaths $ map concat pathsDirections

      mdDeep =   trace(show (_id r) ++ ": " ++ "DEEP   points: " ++ show points ++ " -> dirs: " ++ show (length mappedDirs)) mappedDirs
  in (cache', mdDeep)

runSingleRobotOverAllPaths :: Cache -> Robot -> [String] -> (Cache, [String])
runSingleRobotOverAllPaths cache r =
  foldl' (\(cache', pathsAcc) curPath -> let (cache'', nextPaths) = walkAllPaths cache' r curPath
                                     in (cache'', nextPaths ++ pathsAcc)
         ) (cache, [])


iterateRobots :: Cache -> [Robot] -> [Char] -> (Cache, [String])
iterateRobots _cache [] points = error("there are no robots to run over \"" ++ points ++ "\"")
iterateRobots cache (r:robots) points =
  let (cache', initPaths) = walkAllPaths cache r points
  in foldl' (\(cache'', paths) r' -> runSingleRobotOverAllPaths cache'' r' paths) (cache', initPaths) robots

findScore :: Cache -> [Robot] -> [Char] -> (Cache, Int)
findScore cache robots points =
  let (cache', allPaths) = iterateRobots cache robots points
      minLen = minimum $ map length allPaths
      num = read $ take 3 points
  in trace(points ++ " len: " ++ show minLen ++ ", num: " ++ show num) (cache', num * minLen)

iterateCodes :: Cache -> [Robot] -> [[Char]] -> (Cache, Int)
iterateCodes cache robots =
  foldl' (\(cache', score) code -> let (c', s) = findScore cache' robots code
                                   in (c', score + s)) (cache, 0)

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do
  let numPad = asciiGridMap filterStar "789\n456\n123\n*0A"
  let dirPad = asciiGridMap filterStar "*^A\n<v>"


  let robots = zipWith makeRobot [0..] (numPad : replicate 2 dirPad)
  let codes = X.unpack input
  let codes = "029A\n980A\n179A\n456A\n379A"
  snd $ iterateCodes makeCache robots (lines codes)

solve2 :: ByteString -> Solution
solve2 input = do
  let numPad = asciiGridMap filterStar "789\n456\n123\n*0A"
  let dirPad = asciiGridMap filterStar "*^A\n<v>"


  let robots = zipWith makeRobot [0..] (numPad : replicate 25 dirPad)
  let codes = X.unpack input
  snd $ iterateCodes makeCache robots (lines codes)
