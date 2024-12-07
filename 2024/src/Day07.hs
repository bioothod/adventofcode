module Day07 (solve1, solve2) where

type Solution = Int

runOpsOne :: Int -> Int -> [(Int -> Int -> Int)] -> Int -> [Int]
runOpsOne acc resValue ops e = filter (<= resValue) $ map (\op -> (op acc e)) ops

runOpsOneAcc :: [Int] -> Int -> [(Int -> Int -> Int)] -> Int -> [Int]
runOpsOneAcc acc resValue ops e = concat $ map (\a -> runOpsOne a resValue ops e) acc

runOps :: [Int] -> Int -> [Int -> Int -> Int] -> [Int] -> [Int]
runOps acc resValue ops [] = filter (== resValue) acc
runOps acc resValue ops (e:rest) = let filteredAcc = filter (<= resValue) acc
                                       acc' = runOpsOneAcc filteredAcc resValue ops e
                                   in runOps acc' resValue ops rest

remapValues :: (Read a, Read b) => (String, [Char]) -> (a, [b])
remapValues (rStr, eStrs) = (read rStr, map read (words $ (drop 1 eStrs)))

processLine :: [Int -> Int -> Int] -> [Char] -> Int
processLine ops lineStr = do
  let (resValue, inputElms) = remapValues $ break (==':') lineStr
  let (firstE, rest) = splitAt 1 inputElms
  if elem resValue $ runOps firstE resValue ops rest
    then resValue
    else 0

solve1 :: String -> Solution
solve1 input = do
  let ops = [(+), (*)]
  sum $ map (processLine ops) $ lines input

addDigitOne :: Int -> Int -> Int
addDigitOne a b = 10 * a + b

splitToDigits :: [Int] -> Int -> [Int]
splitToDigits acc a
  | a < 10 = a : acc
  | otherwise = let remA = a `mod` 10
                    baseA = a `div` 10
                in splitToDigits (remA:acc) baseA

joinDigits :: Int -> Int -> Int
joinDigits a b = foldl (\acc b1 -> addDigitOne acc b1) a (splitToDigits [] b)

solve2 :: String -> Solution
solve2 input = do
  let ops = [(+), (*), joinDigits]
  sum $ map (processLine ops) $ lines input
