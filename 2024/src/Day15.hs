module Day15 (solve1, solve2) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Debug.Trace (trace)
import Data.List (intercalate)

data Move = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Show, Eq)

type ObjType = Char
type Vmap = Vector (Vector ObjType)

parseMapLine :: String -> Vector ObjType
parseMapLine = Vector.fromList

getC :: Vmap -> Int -> Int -> ObjType
getC mt x y = let row = mt Vector.! y
              in row Vector.! x

splitAtEmpty :: ([String], [String], Bool) -> String -> ([String], [String], Bool)
splitAtEmpty (wmap, moves, parseWmap) row = do
  let flag' = if parseWmap && null row then False else parseWmap

  if null row
    then (wmap, moves, flag')
    else if parseWmap
         then (row:wmap, moves, flag')
         else (wmap, row:moves, flag')

parseMove :: Char -> Move
parseMove c = case c of
  '^' -> MoveUp
  '>' -> MoveRight
  'v' -> MoveDown
  '<' -> MoveLeft
  _ -> error ("unsupported char " ++ show c)

notEmpty :: Vmap -> Int -> Int -> Bool
notEmpty wmap x y = let p = getC wmap x y
                    in p /= '.' && p /= '#'

updateWmap :: Vmap -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Vmap
updateWmap wmap (sx, sy) (newSx, newSy) (dstX, dstY) = do
  let dstRow = (wmap Vector.! dstY) Vector.// [(dstX, 'O')]
  let dstWmap = wmap Vector.// [(dstY, dstRow)]

  let clearStartRow = (dstWmap Vector.! sy) Vector.// [(sx, '.')]
  let clearStartWmap = dstWmap Vector.// [(sy, clearStartRow)]

  let newStartRow = (clearStartWmap Vector.! newSy) Vector.// [(newSx, '@')]
  let newStartWmap = clearStartWmap Vector.// [(newSy, newStartRow)]

  newStartWmap


moveAll :: Vmap -> (Int, Int) -> Move -> (Vmap, (Int, Int))
moveAll wmap (sx, sy) m = do
  let maxX = Vector.length (wmap Vector.! 0) - 1
  let maxY = Vector.length wmap - 1

  let (newStart, dst) = case m of
        MoveUp -> let r = takeWhile (notEmpty wmap sx) [sy, sy-1..0] in ((sx, sy-1), (sx, last r - 1))
        MoveDown -> let r = takeWhile (notEmpty wmap sx) [sy..maxY] in ((sx, sy+1), (sx, last r + 1))
        MoveLeft -> let r = takeWhile (\x -> notEmpty wmap x sy) [sx, sx-1..0] in ((sx-1, sy), (last r - 1, sy))
        MoveRight -> let r = takeWhile (\x -> notEmpty wmap x sy) [sx..maxX] in ((sx+1, sy), (last r + 1, sy))

  if dst == (sx, sy) || uncurry (getC wmap) dst /= '.'
    then (wmap, (sx, sy))
    else (updateWmap wmap (sx, sy) newStart dst, newStart)

calcGps :: Vmap -> Int
calcGps wmap = sum [100*y+x | (x, y) <- (,) <$> [0..maxX] <*> [0..maxY], getC wmap x y == 'O']
  where maxX = Vector.length (wmap Vector.! 0) - 1
        maxY = Vector.length wmap - 1


type Solution = Int
solve1 :: String -> Solution
solve1 input = do
  let (wmapL, movesL, _) = foldl splitAtEmpty ([], [], True) $ lines input
  let wmap = Vector.fromList $ map parseMapLine $ reverse wmapL
  let moves = map parseMove $ concat $ reverse movesL

  let maxX = Vector.length (wmap Vector.! 0) - 1
  let maxY = Vector.length wmap - 1

  let initialStart = head $ [(x,y) | (x, y) <- (,) <$> [0..maxX] <*> [0..maxY], getC wmap x y == '@']
  let (endWmap, _) = foldl (\(accWmap, start) move -> moveAll accWmap start move) (wmap, initialStart) moves
  trace(intercalate "\n" (map Vector.toList $ Vector.toList endWmap)) calcGps endWmap

solve2 :: String -> Solution
solve2 input = do
  2
