{-# OPTIONS -Wno-unused-top-binds #-}

module Day09 (solve1, solve2) where

import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate, sortBy)

type Solution = Int
type Position = Int
type Size = Int
type Id = Int

data Obj = File Position Size Id
         | Free Position Size
         deriving (Show, Eq, Ord)

objSize :: Obj -> Size
objSize (File _ size _) = size
objSize (Free _ size) = size

objPos :: Obj -> Position
objPos (File pos _ _) = pos
objPos (Free pos _) = pos

objId :: Obj -> Id
objId (File _ _ id') = id'
objId (Free _ _) = error "a free object does not have an ID"


objIdChar :: Obj -> Char
objIdChar (Free _ _) = '.'
objIdChar (File _ _ id') = (intToDigit id')

isFree :: Obj -> Bool
isFree (Free _ _) = True
isFree (File _ _ _) = False

showSym :: Obj -> [Char]
showSym obj = replicate (objSize obj) (objIdChar obj)

createObj :: [Obj] -> Int -> Int -> [Char] -> [Obj]
createObj acc _offset _pos [] = acc
createObj acc offset pos (x:xs) = let size = digitToInt x
                                      newObj = if odd pos
                                               then Free offset size
                                               else File offset size (pos `div` 2)
                                  in createObj (newObj:acc) (offset+size) (pos+1) xs

showObjs :: [Obj] -> [Char]
showObjs objs = intercalate " " $ map showSym objs

parseLine :: [Char] -> [Obj]
parseLine rowStr = reverse $ createObj [] 0 0 rowStr

replaceBlocks :: Obj -> Obj -> (Obj, Obj, [Obj], [Obj])
replaceBlocks oFile oFree = do
  let sizeFile = objSize oFile
  let sizeFree = objSize oFree

  let newFile = File (objPos oFree) (min sizeFile sizeFree) (objId oFile)
  let newFreeBack = Free (objPos oFile) (objSize newFile)

  let restFreeList = if sizeFree > sizeFile
                     then [Free ((objPos newFile) + (objSize newFile)) (sizeFree - (objSize newFile))]
                     else []

  let restFileList = if sizeFile > sizeFree
                     then [File (objPos oFile) (sizeFile - (objSize newFile)) (objId oFile)]
                     else []

  (newFile, newFreeBack, restFreeList, restFileList)

moveOne :: [Obj] -> [Obj] -> [Obj] -> [Obj] -> [Obj]
moveOne left right _ [] = (reverse left) ++ right
moveOne left right [] _ = (reverse left) ++ right
moveOne left right (f:fs) (r:rs)
  | (objPos f) == (objPos r) = r:left
  | (not . isFree) f = moveOne (f:left) right fs (r:rs)
  | isFree r = moveOne left (r:right) (f:fs) rs
  | otherwise = let (newFile, newFree, restFree, restFile) = replaceBlocks r f
                in moveOne (newFile:left) (newFree:right) (restFree++fs) (restFile++rs)

moveBlocks :: [Obj] -> [Obj]
moveBlocks objs = do
  let rev = reverse objs

  moveOne [] [] objs rev

calcObjResult :: Obj -> Int
calcObjResult obj = if isFree obj
                    then 0
                    else sum $ map (* (objId obj)) [(objPos obj)..((objPos obj) + (objSize obj)-1)]

calcualteResult :: [Obj] -> Int
calcualteResult objs = sum $ map calcObjResult objs

solve1 :: String -> Solution
solve1 input = do
  let objs = parseLine $ head $ lines input
  let form = moveBlocks objs
  calcualteResult form

findFreeSlot :: [Obj] -> [Obj] -> Obj -> ([Obj], [Obj])
findFreeSlot ret [] r = (ret, [r])
findFreeSlot ret (l:rest) r
  | objPos r < objPos l = (ret ++ (l:rest), [r])
  | objSize l < objSize r = findFreeSlot (ret++[l]) rest r
  | otherwise = let (newFile, _newFree, restFree, _restFile) = replaceBlocks r l
                in (ret++restFree++rest, [newFile])

sortAll :: [Obj] -> [Obj]
sortAll = sortBy (\a b -> (compare (objPos a) (objPos b)))


moveOneObj :: [Obj] -> [Obj] -> [Obj] -> [Obj]
moveOneObj acc free [] = acc ++ free
moveOneObj acc free (r:rev) = do
  let (left, right) = findFreeSlot [] free r
  moveOneObj (acc++right) left rev

solve2 :: String -> Solution
solve2 input = do
  let objs = parseLine $ head $ lines input
  let free = filter isFree objs
  let rev = filter (not.isFree) $ reverse objs
  let mixed = moveOneObj [] free rev
  calcualteResult $ sortAll $ mixed
