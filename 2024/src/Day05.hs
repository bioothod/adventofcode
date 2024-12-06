module Day05 (solve1, solve2) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortBy)

import Common (wordsWhen)

type Solution = Int

readInput :: [String] -> [String] -> Bool -> [String] -> ([String], [String])
readInput rules pages _ [] = (rules, pages)
readInput rules pages flag (s:rest)
  | s == "" = readInput rules pages True rest
  | flag == False = let new_rules = s:rules in readInput new_rules pages flag rest
  | flag == True = let new_pages = s:pages in readInput rules new_pages flag rest

parseInputRule :: Map String [String] -> String -> Map String [String]
parseInputRule oldMap row = let ([key], [value]) = splitAt 1 $ wordsWhen (=='|') row
                            in Map.insertWith (++) key [value] oldMap

checkBefore :: Map String [String] -> String -> String -> Bool
checkBefore m k v = case Map.lookup k m of
                      Nothing -> False
                      Just l -> v `elem` l

pagesInOrder rules pages =
  and $ zipWith (checkBefore rules) pages (drop 1 pages)

resort rules pages = sortBy func pages
  where func a b = if checkBefore rules a b then LT else GT

getMiddle :: [String] -> Int
getMiddle p = read $ head $ drop (length p `div` 2) p

solve1 :: String -> Solution
solve1 input = do
  let (many_rules, many_pages) = readInput [] [] False $ lines input
  let rules = foldl parseInputRule Map.empty many_rules
  let pages = map (wordsWhen (==',')) many_pages
  sum $ map getMiddle $ filter (pagesInOrder rules) pages

solve2 :: String -> Solution
solve2 input = do
  let (many_rules, many_pages) = readInput [] [] False $ lines input
  let rules = foldl parseInputRule Map.empty many_rules
  let pages = map (wordsWhen (==',')) many_pages
  sum $ map getMiddle $ map (resort rules) $ filter (not . pagesInOrder rules) pages
