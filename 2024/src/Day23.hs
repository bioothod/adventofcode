{-# LANGUAGE  MultiWayIf #-}

module Day23 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as X

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Foldable (foldl', maximumBy)
import Debug.Trace (trace)
import Data.Maybe (fromJust, mapMaybe)
import Data.Function (on)
import Data.List (intercalate, sort)

type Graph = Map ByteString [ByteString]

mergeKeys :: Eq a => [a] -> [a] -> [a]
mergeKeys [] _ = undefined
mergeKeys _ [] = undefined
mergeKeys [key] oldKeys = if key `elem` oldKeys
                          then oldKeys
                          else key:oldKeys

foldGraph :: Graph -> [ByteString] -> Graph
foldGraph acc arg = if length arg /= 2
                    then undefined
                    else let c0 = head arg
                             c1 = arg !! 1
                         in M.insertWith mergeKeys c1 [c0] $ M.insertWith mergeKeys c0 [c1] acc

findDepth :: Graph -> Int -> [[ByteString]] -> [ByteString] -> ByteString -> [[ByteString]]
findDepth graph depth accWhole current key = let updatedCurrent = key:current
                                                 children = fromJust $ M.lookup key graph
                                             in if depth == 0
                                                then updatedCurrent:accWhole
                                                else foldl' (\acc child -> findDepth graph (depth-1) acc updatedCurrent child) accWhole children

findTriplets :: Graph -> ByteString -> Set [ByteString]
findTriplets graph key  = S.fromList $ map sort $ map (drop 1) $ filter (\chain -> head chain == last chain) $ findDepth graph 3 [] [] key


isConnected :: Graph -> ByteString -> Set ByteString -> Bool
isConnected graph k xs = let values = fromJust $ M.lookup k graph
                         in all (`elem` values) xs

pairIsMember :: Ord a => Set [a] -> a -> a -> Bool
pairIsMember s a b = S.member [a, b] s || S.member [b, a] s
pairUpdateChildren :: (Foldable t, Ord a) => Set [a] -> a -> t a -> Set [a]
pairUpdateChildren s a xs = foldl' (\acc k -> S.insert [a, k] $ S.insert [k, a] acc) s xs

findInterconnectedAll :: Graph -> ([Set ByteString], Set [ByteString]) -> Set ByteString -> ByteString -> ([Set ByteString], Set [ByteString])
findInterconnectedAll graph (accWhole, accProcessed) current key =
  let updatedCurrent = S.insert key current
      children = filter (\k -> isConnected graph k updatedCurrent) $ filter (not . pairIsMember accProcessed key) $ fromJust $ M.lookup key graph
      updatedProcessed = pairUpdateChildren accProcessed key children
  in if null children
     then (updatedCurrent:accWhole, updatedProcessed)
     else foldl' (\acc child -> findInterconnectedAll graph acc updatedCurrent child) (accWhole, updatedProcessed) children

findInterconnected :: Graph -> ([Set ByteString], Set [ByteString]) -> ByteString -> ([Set ByteString], Set [ByteString])
findInterconnected graph acc key = findInterconnectedAll graph acc S.empty key

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do
  let pairs = map (X.split '-') $ X.lines input
  let graph = foldl' foldGraph M.empty pairs
  let triplets = S.unions $ map (findTriplets graph) $ M.keys graph
  let tMatching = S.filter (any (\x -> X.head x == 't')) triplets
  length tMatching

solve2 :: ByteString -> Solution
solve2 input = do
  let pairs = map (X.split '-') $ X.lines input
  let graph = foldl' foldGraph M.empty pairs
  let inter = fst $ foldl' (findInterconnected graph) ([], S.empty) $ M.keys graph
  let interconnected = S.fromList $ map (sort . S.toList) inter
  let longest = maximumBy (compare `on` length) interconnected
  trace(intercalate "," (map X.unpack longest)) 1
