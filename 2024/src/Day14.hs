module Day14 (solve1, solve2) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Common (Coord(..))
import Parser (Parser, parseInt)
--import Debug.Trace (trace) -- to show the Christmas tree
import Data.List (intercalate)

type Solution = Int

parseCoord :: Parser Coord
parseCoord = do
  x <- parseInt
  _ <- char ','
  y <- parseInt
  return (Coord x y)

data Robot = Robot {
  coords :: Coord,
  vel :: Coord
} deriving Show

parseLine :: Parser Robot
parseLine = do
  _ <- string "p="
  c <- parseCoord
  _ <- string " v="
  v <- parseCoord
  _ <- optional eol
  return (Robot c v)

parseManyRobots :: Parser [Robot]
parseManyRobots = many parseLine

parseRobots :: String -> [Robot]
parseRobots input = case runParser parseManyRobots "robots" input of
                      Left e -> error (errorBundlePretty e)
                      Right x -> x

type Limit = Coord
makeStep :: Limit -> Int -> Robot -> Robot
makeStep (Coord sizeX sizeY) step (Robot (Coord x y) (Coord vx vy)) = Robot newC (Coord vx vy)
  where x' = (x + step * vx) `mod` sizeX
        y' = (y + step * vy) `mod` sizeY
        newC = Coord x' y'

solve1 :: String -> Solution
solve1 input = do
  let robots = parseRobots input
  let maxC = Coord 101 103
  let maxStep = 100
  let movedRobots = moveRobots maxC maxStep robots
  let middleX = (coordX maxC) `div` 2
  let middleY = (coordY maxC) `div` 2
  let q0 = length $ filter (\(Robot (Coord x y) _) -> (x < middleX) && (y < middleY)) movedRobots
  let q1 = length $ filter (\(Robot (Coord x y) _) -> (x > middleX) && (y < middleY)) movedRobots
  let q2 = length $ filter (\(Robot (Coord x y) _) -> (x < middleX) && (y > middleY)) movedRobots
  let q3 = length $ filter (\(Robot (Coord x y) _) -> (x > middleX) && (y > middleY)) movedRobots
  q0 * q1 * q2 * q3

type CoordSet = Set Coord
dotOrNot :: CoordSet -> Int -> Int -> Char
dotOrNot cmap x y = if Set.member (Coord x y) cmap
                    then '*'
                    else '.'

makeRset :: [Robot] -> CoordSet
makeRset = foldl (\acc (Robot c _) -> Set.insert c acc) Set.empty

showRobots :: [Robot] -> Limit -> String
showRobots robots (Coord sizeX sizeY) = do
  let rmap = makeRset robots
  let dots = map (\y -> map (\x -> dotOrNot rmap x y) [0..(sizeX-1)]) [0..(sizeY-1)]
  intercalate "\n" dots

rowIncluded :: CoordSet -> [Coord] -> Bool
rowIncluded cmap coords' = all (\c -> Set.member c cmap) coords'

makeRows :: Coord -> Int -> ([Coord], [Coord])
makeRows (Coord x y) steps = (hor, ver)
  where hor = map (\s -> Coord (x+s) y) [0..(steps-1)]
        ver = map (\s -> Coord x (y+s)) [0..(steps-1)]

partOfSomething :: CoordSet -> Int -> Coord -> Bool
partOfSomething cmap steps c = let (hor, ver) = makeRows c steps
                               in rowIncluded cmap hor || rowIncluded cmap ver

hasRow :: [Robot] -> Int -> Bool
hasRow robots rnum = do
  let cmap = makeRset robots
  (not.null) $ dropWhile (\(Robot c _) -> not $ partOfSomething cmap rnum c) robots

moveRobots :: Limit -> Int -> [Robot] -> [Robot]
moveRobots maxC steps = map (makeStep maxC steps)

solve2 :: String -> Solution
solve2 input = do
  let robots = parseRobots input
  let maxC = Coord 101 103
  let rnum = 8 -- selected by hands by watching shorter length matches
  let ((movedRobots, steps), _) = head $ dropWhile (not.snd) $ map (\numSteps -> let r = moveRobots maxC numSteps robots in ((r, numSteps), hasRow r rnum)) [0..]
  --trace(showRobots movedRobots maxC) steps
  steps
