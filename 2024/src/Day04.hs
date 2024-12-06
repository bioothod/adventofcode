module Day04 (solve1, solve2) where

type Solution = Int

rl :: [[a]] -> [[a]]
rl [] = []
rl ([]:_) = []
rl m = map last m : (rl (map init m))

rotNum :: Integer -> [[[Char]]] -> [[Char]] -> [[[Char]]]
rotNum n ls m
  | n == 0 = m:ls
  | otherwise = let nls = m:ls
                in rotNum (n-1) nls (rl m)

makeRow :: Int -> Int -> [Char] -> [Char]
makeRow start n row = drop (n + start) row ++ replicate n '*'

diag :: Int -> [[Char]] -> [[Char]]
diag start m = zipWith (makeRow start) [0..] m

reverseM :: [[a]] -> [[a]]
reverseM m = map reverse m

countRow :: String -> [Bool] -> [Char] -> [Bool]
countRow _ acc [] = acc
countRow xmas acc (x:xs) = let row = (x:xs)
                               acc1 = ((take (length xmas) row) == xmas):acc
                           in countRow xmas acc1 xs

formM :: [[a]] -> Int -> [a] -> [[a]]
formM mout _ [] = mout
formM mout n minN = let row = take n minN
                        rest = drop n minN
                   in row : formM mout n rest

countM :: [[Char]] -> Int
countM m = sum $ map length $ map (filter (==True)) $ map (countRow "XMAS" []) m

search :: [Int] -> [a] -> [a]
search indices xs = go indices 0 xs  -- start from index 0
   where
   go :: [Int] -> Int -> [a] -> [a]
   -- no more indices, we are done
   go []     _ _                = []
   -- more indices but no more elements -> error
   go _      _ []               = error "index not found"
   -- if the wanted index i is the same as the current index j,
   -- return the current element y, more to the next wanted index
   go (i:is) j yys@(y:_) | i==j = y : go is  j     yys
   -- otherwise, skip y and increment the current index j
   go iis    j (_:ys)           =     go iis (j+1) ys

cmpLetters :: (Eq a) => a -> a -> a -> Bool
cmpLetters a b wildcard
  | a == wildcard || b == wildcard = True
  | otherwise = a == b

rowFindSeq :: (Eq a, Num b) => a -> [b] -> b -> [a] -> [a] -> [b]
rowFindSeq _ acc _ _ [] = acc
rowFindSeq _ _ _ (_:_:_:_:_) _ = error "seq must have length 3"
rowFindSeq _ _ _ [_,_] _ = error "seq must have length 3"
rowFindSeq _ _ _ [_] _ = error "seq must have length 3"
rowFindSeq _ acc _ _ [_] = acc
rowFindSeq _ acc _ _ [_,_] = acc
rowFindSeq _ _ _ [] _ = error "matching sequence must be provided"
rowFindSeq wildcard acc pos [a,b,c] (xa:xb:xc:xs) = if (cmpLetters xa a wildcard) && (cmpLetters xb b wildcard) && (cmpLetters xc c wildcard)
                                                    then let acc1 = acc++[pos]
                                                         in rowFindSeq wildcard acc1 (pos+1) [a,b,c] (xb:xc:xs)
                                                    else rowFindSeq wildcard acc (pos+1) [a,b,c] (xb:xc:xs)

matrixFindSeq :: (Eq a) => a -> [a] -> [[a]] -> [[Int]]
matrixFindSeq wildcard l = map (rowFindSeq wildcard [] 0 l)

dropMin :: Eq a => a -> [a] -> [a]
dropMin _ [] = []
dropMin mv (r:row) = if mv == r
                     then row
                     else r:row

matchRows :: (Num a, Num t, Ord a) => t -> [a] -> [a] -> [a] -> t
matchRows acc [] _ _ = acc
matchRows acc _ [] _ = acc
matchRows acc _ _ [] = acc
matchRows acc (m0:m0s) (a:as) (m1:m1s) = if (m0 == m1) && (a == m0)
                                         then matchRows (acc+1) m0s as m1s
                                         else let minall = minimum [m0, a, m1]
                                                  m0su = dropMin minall (m0:m0s)
                                                  asu = dropMin minall (a:as)
                                                  m1su = dropMin minall (m1:m1s)
                                              in matchRows acc m0su asu m1su



matrixMatch :: [[Char]] -> Int
matrixMatch m = do
  let wildcard = '*'
  let pM_S = matrixFindSeq wildcard "M*S" m
  let p_A_ = matrixFindSeq wildcard "*A*" m
  let matches = sum $ zipWith3 (matchRows 0) pM_S (drop 1 p_A_) (drop 2 pM_S)
  matches



solve1 :: String -> Solution
solve1 input = do
  let m0 = [lines input]
  let rm0 = foldl (\acc m -> (reverseM m):acc) [] m0
  let m2 = foldl (\acc m -> (rotNum 3 [] m) ++ acc) [] m0
  let rm2 = foldl (\acc m -> (rotNum 3 [] m) ++ acc) [] rm0
  let m3 = foldl (\acc m -> (rl $ diag 0 m):acc) [] m2
  let rm3 = foldl (\acc m -> (rl $ diag 1 m):acc) [] rm2

  sum $ map countM $ m3 ++ m2 ++ rm3

solve2 :: String -> Solution
solve2 input = do
  let m0 = [lines input]
  let m2 = foldl (\acc m -> (rotNum 3 [] m) ++ acc) [] m0
  sum $ map matrixMatch m2
