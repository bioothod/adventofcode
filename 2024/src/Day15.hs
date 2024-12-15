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

moveObj :: (Int, Int) -> Move -> (Int, Int)
moveObj (x, y) m = case m of
  MoveUp -> (x, y-1)
  MoveDown -> (x, y+1)
  MoveLeft -> (x-1, y)
  MoveRight -> (x+1, y)

map2str :: Vmap -> [Char]
map2str m = intercalate "\n" $ map Vector.toList $ Vector.toList m

notEmpty :: Vmap -> Int -> Int -> Bool
notEmpty wmap x y = let p = getC wmap x y
                    in p /= '.' && p /= '#'

markObj :: Vmap -> ObjType -> (Int, Int) -> Vmap
markObj wmap obj (x, y) = do
  let row = (wmap Vector.! y) Vector.// [(x, obj)]
  wmap Vector.// [(y, row)]

updateWmap :: Vmap -> [(Int, Int)] -> Move -> (Vmap, (Int, Int))
updateWmap wmap positions m = do
  let newPositions = map (flip moveObj m) positions
  let objs = map (uncurry (getC wmap)) positions

  let wmapClear = foldl (\acc pos -> markObj acc '.' pos) wmap positions
  let newWmap = foldl (\acc (obj, pos) -> markObj acc obj pos) wmapClear (zip objs newPositions)
  let newStart = head newPositions
  (newWmap, newStart)

findObjectsToMove :: Vmap -> (Int -> Int -> Int) -> [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], Bool)
findObjectsToMove _wmap _ acc (_, 0) = (acc, False)
findObjectsToMove _wmap _ acc (0, _) = (acc, False)
findObjectsToMove wmap func acc start@(sx, sy) = do
  let y = sy `func` 1

  case getC wmap sx y of
    c
      | c == '#' -> ([], False)
      | c == '.' -> (start:acc, True)
      | c `elem` "O@" -> let (acc', canMove) = findObjectsToMove wmap func acc (sx, y)
                         in if canMove
                            then ([start]++acc'++acc, canMove)
                            else (acc, False)
      | c `elem` "[]" -> let (pr, pl) = case c of
                               ']' -> ((sx, y), (sx-1, y))
                               '[' -> ((sx+1, y), (sx, y))
                               _ -> undefined

                             (accL, canMoveL) = findObjectsToMove wmap func acc pl
                             (accR, canMoveR) = findObjectsToMove wmap func acc pr
                         in if canMoveL && canMoveR
                            then ([start]++accL++accR++acc, True)
                            else (acc, False)
      | True -> undefined

moveAll :: Vmap -> (Int, Int) -> Move -> (Vmap, (Int, Int))
moveAll wmap start@(sx, sy) m = do
  let maxX = Vector.length (wmap Vector.! 0) - 1

  let (positions, canMove) = case m of
        MoveUp -> findObjectsToMove wmap (-) [] start
        MoveDown -> findObjectsToMove wmap (+) [] start
        MoveLeft -> let r = takeWhile (\(x, _) -> notEmpty wmap x sy) [(x, sy) | x <- [sx, sx-1..0]]; (lx, ly) = last r in (r, getC wmap (lx-1) ly == '.')
        MoveRight -> let r = takeWhile (\(x, _) -> notEmpty wmap x sy) [(x, sy) | x <- [sx..maxX]]; (lx, ly) = last r in (r, getC wmap (lx+1) ly == '.')

  if not canMove
    then (wmap, (sx, sy))
    else updateWmap wmap positions m

calcGps :: Vmap -> Int
calcGps wmap = sum [100*y+x | (x, y) <- (,) <$> [0..maxX] <*> [0..maxY], (getC wmap x y) `elem` "[O"]
  where maxX = Vector.length (wmap Vector.! 0) - 1
        maxY = Vector.length wmap - 1

extendRow :: (Char -> String) -> String -> String
extendRow func = concatMap func


solveGeneric :: String -> (Char -> String) -> Int
solveGeneric input charMapFunc = do
  let (wmapL, movesL, _) = foldl splitAtEmpty ([], [], True) $ lines input
  let wmap = Vector.fromList $ map parseMapLine $ map(extendRow charMapFunc) $ reverse wmapL
  let moves = map parseMove $ concat $ reverse movesL

  let maxX = Vector.length (wmap Vector.! 0) - 1
  let maxY = Vector.length wmap - 1

  let initialStart = head $ [(x,y) | (x, y) <- (,) <$> [0..maxX] <*> [0..maxY], getC wmap x y == '@']
  let (endWmap, _) = foldl (\(accWmap, start) move -> moveAll accWmap start move) (wmap, initialStart) moves
  calcGps endWmap

noExtension :: Char -> String
noExtension x = [x]

type Solution = Int
solve1 :: String -> Solution
solve1 input = do
  solveGeneric input noExtension

extChar :: Char -> String
extChar c = case c of
  '#' -> "##"
  'O' -> "[]"
  '.' -> ".."
  '@' -> "@."
  _ -> error ("unsupported symbol " ++ show c)

solve2 :: String -> Solution
solve2 input = do
  solveGeneric input extChar
