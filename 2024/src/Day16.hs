module Day16 (solve1, solve2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Common (Coord(..))
import Data.Maybe (isJust, fromMaybe)

type EmptyMap = Set Coord
type WallsMap = Set Coord

type AccumulatorTuple = (EmptyMap, WallsMap, Coord, Coord)

data Direction = North | West | South | East deriving (Show, Eq, Ord, Enum)

insertCoord :: AccumulatorTuple -> Coord -> Char -> AccumulatorTuple
insertCoord (eset, wset, s, e) coord char
  | char == '.' = (Set.insert coord eset, wset, s, e)
  | char == 'S' = (Set.insert coord eset, wset, coord, e)
  | char == 'E' = (Set.insert coord eset, wset, s, coord)
  | char == '#' = (eset, Set.insert coord wset, s, e)
  | otherwise = error ("unsupported char " ++ show char ++ " at " ++ show coord)


parseLine :: AccumulatorTuple -> Int -> String -> AccumulatorTuple
parseLine accTuple y row =
  foldl (\acc (x, char) -> insertCoord acc (Coord x y) char) accTuple (zip [0..] row)

data Action = Move | TurnLeft | TurnRight deriving (Eq, Show)
type Step = (Coord, Action, Direction)

calcNextPos :: Coord -> Direction -> Coord
calcNextPos (Coord x y) dir = case dir of
  North -> Coord x (y-1)
  South -> Coord x (y+1)
  West -> Coord (x-1) y
  East -> Coord (x+1) y

getOptions :: EmptyMap -> Coord -> Direction -> [Step]
getOptions eMap pos dir = do
  let npos = calcNextPos pos dir
  let rotActions = [(pos, TurnLeft, dir), (pos, TurnRight, dir)]
  if Set.member npos eMap
    then (pos, Move, dir):rotActions
    else rotActions

changeDirection :: Direction -> Action -> Direction
changeDirection dir action = case dir of
  North -> if action == TurnLeft then West else East
  South -> if action == TurnLeft then East else West
  East -> if action == TurnLeft then North else South
  West -> if action == TurnLeft then South else North

takeAction :: Step -> (Action, Coord, Direction)
takeAction (pos, action, dir) = case action of
  Move -> (action, calcNextPos pos dir, dir)
  _ -> (action, pos, changeDirection dir action)

opposite :: Direction -> Direction
opposite action = case action of
  North -> South
  South -> North
  East -> West
  West -> East

scoreAction :: Num a => Action -> a
scoreAction a = if a == Move then 1 else 1000

type ScoreMap = Map (Coord, Direction) Int

makeOneStep :: AccumulatorTuple -> (ScoreMap, Int) -> Int -> Coord -> Direction -> (ScoreMap, Int)
makeOneStep aTuple@(eSet, _, _, end) (scoreMap, bestScore) currentScore pos dir = do
  let savedScore = fromMaybe 0 (Map.lookup (pos, dir) scoreMap)
  let scoreMap' = Map.insert (pos, dir) currentScore scoreMap
  let nextSteps = map takeAction $ getOptions eSet pos dir

  if (currentScore >= savedScore && savedScore /= 0) || (currentScore > bestScore)
    then (scoreMap, bestScore)
    else if pos == end || null nextSteps
         then (scoreMap', min bestScore currentScore)
         else foldl (\scoreAcc (action, nextPos, nextDir) ->
                        makeOneStep aTuple scoreAcc (currentScore+scoreAction action) nextPos nextDir)
              (scoreMap', bestScore) nextSteps

type Solution = Int
solve1 :: String -> Solution
solve1 input = do
  let aTuple@(_, _, start, _end) = foldl (\acc (y, row) -> parseLine acc y row) (Set.empty, Set.empty, Coord 0 0, Coord 0 0) (zip [0..] (lines input))
  let (_, bestScore) = makeOneStep aTuple (Map.empty, maxBound) 0 start East
  bestScore

findMinAndStep :: ScoreMap -> Int -> EmptyMap -> Coord -> Coord -> EmptyMap
findMinAndStep scoreMap currentScore nodeMap pos start = do
  let dirScoreListMaybe = filter (\(_, x) -> isJust x) $ map (\dest-> (dest, Map.lookup (pos, dest) scoreMap)) [North ..]
  let dirScoreListAll = filter (\(_, x) -> x == currentScore || x == currentScore-1000) $ map (\(sc, Just x) -> (sc, x)) dirScoreListMaybe
  let dirScoreList = map (\(dir, score) -> if score == currentScore then (dir, currentScore) else (dir, currentScore-1000)) dirScoreListAll

  let nodeMap' = if null dirScoreList then nodeMap else Set.insert pos nodeMap

  if pos == start
  then Set.insert pos nodeMap'
  else foldl (\acc (dir, score) -> findMinAndStep scoreMap (score-1) acc (calcNextPos pos (opposite dir)) start) nodeMap' dirScoreList

solve2 :: String -> Solution
solve2 input = do
  let aTuple@(_, _, start, end) = foldl (\acc (y, row) -> parseLine acc y row) (Set.empty, Set.empty, Coord 0 0, Coord 0 0) (zip [0..] (lines input))
  let (scoreMap, bestScore) = makeOneStep aTuple (Map.empty, maxBound) 0 start East
  length $ findMinAndStep scoreMap bestScore Set.empty end start
