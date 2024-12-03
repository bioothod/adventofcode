module Day02 (solve1, solve2) where

type Solution = Integer

num_words :: [String] -> [Integer]
num_words [] = []
num_words ls = map read ls

max_step = 3
min_step = 1
check_pair_decr :: Integer -> Integer -> Bool
check_pair_decr a b = b <= a - min_step && b >= a - max_step

check_pair_acc :: (Integer, Bool) -> Integer -> (Integer, Bool)
check_pair_acc (acc_value, acc_bool) next = (next, bool_result)
  where bool_result = (&&) acc_bool $ check_pair_decr acc_value next


is_safe_decr :: [Integer] -> Bool
is_safe_decr [] = False
is_safe_decr ls = bool_result
 where (_, bool_result) = foldl check_pair_acc (start, True) rest
        where ([start], rest) = splitAt 1 ls

is_safe :: [Integer] -> Bool
is_safe [] = False
is_safe report = (is_safe_decr report) || (is_safe_decr $ reverse report)

solve1 :: String -> Solution
solve1 input = do
  let reports = map num_words . map words $ lines input
  let safe = filter is_safe reports
  toInteger $ length safe

solve2 :: String -> Solution
solve2 input = do
  2
