module Day12 (solve1, solve2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Common (foldl')
import Data.Maybe (catMaybes, fromMaybe)

data Coord = Coord {
    coordX :: Int
  , coordY :: Int
} deriving (Show, Eq, Ord)

type Plant = Char
data Plot = Plot Plant Coord Id
          deriving Show

coords :: Plot -> Coord
coords (Plot _ c _) = c

plantType :: Plot -> Plant
plantType (Plot p _ _) = p

plotId :: Plot -> Id
plotId (Plot _ _ id') = id'

type Id = Int
type GardenMap = Map Coord Plot
type Regions = Map Id GardenMap

defaultId :: Id
defaultId = -1
createPlot :: Int -> Int -> Char -> Plot
createPlot y x t = Plot t (Coord x y) defaultId

parseRow :: Int -> String -> [Plot]
parseRow y = zipWith (createPlot y) [0..]

possibleNeighbours :: GardenMap -> Coord -> [Plot]
possibleNeighbours gardenMap (Coord cx cy) =
  catMaybes $ map (\c -> Map.lookup c gardenMap) $ [Coord (cx+1) cy, Coord (cx-1) cy, Coord cx (cy+1), Coord cx (cy-1)]

makeGardenStart :: GardenMap -> GardenMap -> Id -> Plot -> (GardenMap, GardenMap)
makeGardenStart accMap gardenMap regionId start = do
  let start' = Plot (plantType start) (coords start) regionId
  let accMap' = Map.insert (coords start) start' accMap
  let gardenMap' = Map.delete (coords start) gardenMap

  let neighbours = possibleNeighbours gardenMap (coords start)
  let sameTypeNeighbours = filter (\p -> (plantType p) == (plantType start)) neighbours

  foldl' (\(acc, garden) c -> makeGardenStart acc garden regionId c) (accMap', gardenMap') sameTypeNeighbours

makeGarden :: GardenMap -> GardenMap -> Id -> (GardenMap, GardenMap, Id)
makeGarden accMap gardenMap regionId = do
  if Map.size gardenMap == 0
    then (accMap, gardenMap, regionId)
    else let start = (snd.head) (Map.toList gardenMap)
             (accMap', gardenMap') = makeGardenStart accMap gardenMap regionId start
         in makeGarden accMap' gardenMap' (regionId+1)

regionsUpdate :: Regions -> Plot -> Regions
regionsUpdate regions plot = let gmap = Map.insert (coords plot) plot $ fromMaybe Map.empty (Map.lookup (plotId plot) regions)
                             in Map.insert (plotId plot) gmap regions


makeRegions :: GardenMap -> Regions
makeRegions gardenMap = foldl' (\reg (_c, p) -> regionsUpdate reg p) Map.empty (Map.toList gardenMap)

getPerimeter :: GardenMap -> Plot -> Int
getPerimeter gardenMap plot = 4 - (length $ possibleNeighbours gardenMap (coords plot))

sumPerimeter :: GardenMap -> Int -> Plot -> (Int, Plot)
sumPerimeter gardenMap acc plot = (acc + getPerimeter gardenMap plot, plot)

calcFence :: GardenMap -> Int
calcFence gardenMap = let p = fst $ Map.mapAccum (sumPerimeter gardenMap) 0 gardenMap
                          a = Map.size gardenMap
                      in p * a

isEmpty :: GardenMap -> Coord -> Bool
isEmpty gardenMap c = case Map.lookup c gardenMap of
  Nothing -> True
  Just _ -> False

isOnSideUp :: GardenMap -> Int -> Int -> Bool
isOnSideUp gardenMap x y = let u = Coord x (y-1)
                               l = Coord (x-1) y
                               a = Coord (x-1) (y-1)
                               self = Coord x y
                               s1 = isEmpty gardenMap l
                               s2 = (not $ isEmpty gardenMap a) && (not $ isEmpty gardenMap l)
                           in (not $ isEmpty gardenMap self) && (isEmpty gardenMap u) && (s1 || s2)

isOnSideDown :: GardenMap -> Int -> Int -> Bool
isOnSideDown gardenMap x y = let l = Coord (x-1) y
                                 d = Coord x (y+1)
                                 a = Coord (x-1) (y+1)
                                 self = Coord x y
                                 s1 = isEmpty gardenMap l
                                 s2 = (not $ isEmpty gardenMap l) && (not $ isEmpty gardenMap a)
                             in (not $ isEmpty gardenMap self) && (isEmpty gardenMap d) && (s1 || s2)

isOnSideLeft :: GardenMap -> Int -> Int -> Bool
isOnSideLeft gardenMap x y = let l = Coord (x-1) y
                                 d = Coord x (y+1)
                                 a = Coord (x-1) (y+1)
                                 self = Coord x y
                                 s1 = isEmpty gardenMap d
                                 s2 = (not $ isEmpty gardenMap d) && (not $ isEmpty gardenMap a)
                             in (not $ isEmpty gardenMap self) && (isEmpty gardenMap l) && (s1 || s2)

isOnSideRight :: GardenMap -> Int -> Int -> Bool
isOnSideRight gardenMap x y = let r = Coord (x+1) y
                                  d = Coord x (y+1)
                                  a = Coord (x+1) (y+1)
                                  self = Coord x y
                                  s1 =  isEmpty gardenMap d
                                  s2 = (not $ isEmpty gardenMap d) && (not $ isEmpty gardenMap a)
                              in (not $ isEmpty gardenMap self) && (isEmpty gardenMap r) && (s1 || s2)

countSides :: (GardenMap -> Int -> Int -> Bool) -> GardenMap -> Int -> Int -> Int -> Int
countSides f gardenMap y minX maxX =
  length $ filter (\x -> f gardenMap x y) [minX..maxX]

getSides :: GardenMap -> Int
getSides gardenMap = do
  let mx = map (\(Coord x _y, _) -> x) $ Map.toList gardenMap
  let minX = minimum mx
  let maxX = maximum mx
  let my = map (\(Coord _x y, _) -> y) $ Map.toList gardenMap
  let minY = minimum my
  let maxY = maximum my

  let up = sum $ map (\y -> countSides isOnSideUp gardenMap y minX maxX) [minY..maxY]
  let down = sum $ map (\y -> countSides isOnSideDown gardenMap y minX maxX) [minY..maxY]

  let left = sum $ map (\y -> countSides isOnSideLeft gardenMap y minX maxX) [minY..maxY]
  let right = sum $ map (\y -> countSides isOnSideRight gardenMap y minX maxX) [minY..maxY]

  let area = Map.size gardenMap
  (*) area $ up + down + left + right

type Solution = Int

solve1 :: String -> Solution
solve1 input = do
  let gardenRaw = foldl (\acc p -> Map.insert (coords p) p acc) Map.empty $ concat $ zipWith parseRow [0..] $ lines input
  let (garden, _, _) = makeGarden Map.empty gardenRaw 0
  let regions = makeRegions garden
  let fences = Map.map calcFence regions
  sum $ map snd $ Map.toList fences

solve2 :: String -> Solution
solve2 input = do
  let gardenRaw = foldl (\acc p -> Map.insert (coords p) p acc) Map.empty $ concat $ zipWith parseRow [0..] $ lines input
  let (garden, _, _) = makeGarden Map.empty gardenRaw 0
  let regions = makeRegions garden
  let sides = Map.map getSides regions
  sum $ map snd $ Map.toList sides
