module Common.Utils (
  wordsWhen
  , middle
  , Coord(..)
) where

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

data Coord = Coord {
  coordX :: Int,
  coordY :: Int
} deriving (Eq, Show, Ord)
