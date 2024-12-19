module Day19 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as XS

import Data.Trie (Trie)
import qualified Data.Trie as Trie

import Data.Maybe (fromMaybe)

type PatternMap = Trie Int

listToTuple :: t -> [a] -> [(a, t)]
listToTuple _ [] = []
listToTuple smth [x] = [(x, smth)]
listToTuple smth (x:xs) = (x, smth):listToTuple smth xs

alterCache :: ByteString -> Int -> Maybe Int -> Maybe Int
alterCache _key newValue oldValue = Just (newValue + fromMaybe 0 oldValue)


isPossible :: (Int, PatternMap) -> PatternMap -> ByteString -> (Int, PatternMap)
isPossible acc@(accNum, cache) patterns design =
  case Trie.lookup design cache of
    Just n -> (accNum + n, cache)
    Nothing -> foldl (\acc'@(accNum', cache') (_, _, rest) -> if BS.null rest
                                                              then (accNum' + 1, Trie.alterBy alterCache design 1 cache')
                                                              else let updatedAcc@(updatedNum, updatedCache) = isPossible acc' patterns rest
                                                                       diff = updatedNum - accNum'
                                                                   in if diff == 0
                                                                      then updatedAcc
                                                                      else (updatedNum, Trie.alterBy alterCache design diff updatedCache)
                     ) acc $ Trie.matches patterns design

type Solution = Int

solve1 :: ByteString -> Solution
solve1 input = do
  let inputLines = XS.lines input
  let patterns = Trie.fromList $ listToTuple 1 $ map XS.strip $ XS.split ',' $ head inputLines
  let designs = drop 2 inputLines
  length $ filter ((/= 0).fst) $ map (isPossible (0, Trie.empty) patterns) designs

solve2 :: ByteString -> Solution
solve2 input = do
  let inputLines = XS.lines input
  let patterns = Trie.fromList $ listToTuple 1 $ map XS.strip $ XS.split ',' $ head inputLines
  let designs = drop 2 inputLines
  fst $ foldl (\acc design -> isPossible acc patterns design) (0, Trie.empty) designs
