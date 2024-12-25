module Common.Utils (module Common.Utils) where

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

data Coord = Coord {
  coordX :: !Int,
  coordY :: !Int
} deriving (Eq, Show, Ord)

possibleCoords :: Coord -> [Coord]
possibleCoords (Coord x y) = [Coord (x+1) y, Coord (x-1) y, Coord x (y+1), Coord x (y-1)]

checkBoundaries :: (Int, Int) -> Coord -> Bool
checkBoundaries (maxX, maxY) (Coord cx cy) = cx >= 0 && cx <= maxX && cy >= 0 && cy <= maxY

genNextStepsCheck :: (Coord -> Bool) -> Coord -> [Coord]
genNextStepsCheck check pos = filter check $ possibleCoords pos

genNextStepsCheckWeight :: (Coord -> Bool) -> (Coord -> (Coord, Int)) -> Coord -> [(Coord, Int)]
genNextStepsCheckWeight check weight pos = map weight $ filter check $ possibleCoords pos


findSub :: (Eq a) => [a] -> [a] -> Maybe Int
findSub sub s
  | length sub > length s      = Nothing
  | take (length sub) s == sub = Just 0
  | otherwise                  = fmap (+1) $ findSub sub $ drop 1 s
