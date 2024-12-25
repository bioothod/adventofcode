{-# LANGUAGE  MultiWayIf #-}

module Day22 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as X

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Function (on)
import Data.Bits (xor)
import Data.Maybe (mapMaybe)
import Data.Foldable (maximumBy)

stage1 :: Int -> Int
stage1 n = let m = n * 64
               t = n `xor` m
           in t `mod` 16777216

stage2 :: Int -> Int
stage2 n = let m = n `div` 32
               t = m `xor` n
           in t `mod` 16777216

stage3 :: Int -> Int
stage3 n = let m = n * 2048
               t = m `xor` n
            in t `mod` 16777216

stageLoop :: Int -> Int -> [Int]
stageLoop n m = take m $ iterate (stage3 . stage2 . stage1) n

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do
  let numbers = map fst $ mapMaybe X.readInt $ X.lines input
  let secrets = map (\n -> last $ stageLoop n 2001) numbers
  sum secrets

mapChanges :: Num c => [c] -> [c]
mapChanges row = zipWith (-) (drop 1 row) row

mapSequence :: Map [Int] Int -> [Int] -> [Int] -> Map [Int] Int
mapSequence acc prices changes = let prefix = take 4 changes
                                     price = head prices
                                     nextPrices = drop 1 prices
                                     nextChanges = drop 1 changes
                                 in if | length prefix /= 4 -> acc
                                       | M.member prefix acc -> mapSequence acc nextPrices nextChanges
                                       | otherwise -> mapSequence (M.insert prefix price acc) nextPrices nextChanges

solve2 :: ByteString -> Solution
solve2 input = do
  let numbers = map fst $ mapMaybe X.readInt $ X.lines input
  let secrets = map (`stageLoop` 2001) numbers
  let prices = map (map (`mod` 10)) secrets
  let changes = map mapChanges prices
  let smaps = zipWith (\p c -> mapSequence M.empty (drop 4 p) c) prices changes
  let allKeys = M.unionsWith (+) smaps
  let (_maxKey, maxVal) = maximumBy (compare `on` snd) $ M.toList allKeys
  maxVal
