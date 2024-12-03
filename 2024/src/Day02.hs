module Day02 (solve1, solve2) where

type Solution = Integer

num_words :: [String] -> [Integer]
num_words [] = []
num_words ls = map read ls

max_step :: Integer
max_step = 3
min_step :: Integer
min_step = 1
check_pair_decr :: (Integer, Integer) -> Bool
check_pair_decr (a, b) = b <= a - min_step && b >= a - max_step

check_pairs_acc :: [Integer] -> [Bool]
check_pairs_acc xs = map check_pair_decr . zip xs $ drop 1 xs

try_and_drop :: Int -> [Integer] -> Bool
try_and_drop x xs = let (left, right) = splitAt x xs
                        new_list = left ++ (drop 1 right)
                    in and $ check_pairs_acc new_list

find_one_dropped :: [Bool] -> [Integer] -> Bool
find_one_dropped _bools xs = or $ map (\x -> try_and_drop x xs) [0..(length xs)-1]

drop_one_and_check :: Bool -> [Integer] -> Bool
drop_one_and_check strict xs = let bools = check_pairs_acc xs
                                   num_failures = length $ filter (not) bools
                               in case num_failures of
                                    0 -> True
                                    _ -> if strict
                                         then False
                                         else find_one_dropped bools xs

is_safe_drop :: Bool -> [Integer] -> Bool
is_safe_drop strict report = (drop_one_and_check strict report) || (drop_one_and_check strict $ reverse report)

solve1 :: String -> Solution
solve1 input = do
  let reports = map num_words . map words $ lines input
  let safe = filter (is_safe_drop True) reports
  toInteger $ length safe

solve2 :: String -> Solution
solve2 input = do
  let reports = map num_words . map words $ lines input
  let safe = filter (is_safe_drop False) reports
  toInteger $ length safe
