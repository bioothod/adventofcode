module Common (
  wordsWhen
  , middle
  , foldl'
) where

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs


foldl' :: (t -> a -> t) -> t -> [a] -> t
foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs
