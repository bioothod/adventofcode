{-# LANGUAGE MultiWayIf #-}

module Day24 (solve1, solve2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as X
import Data.Maybe (fromJust, mapMaybe)
import Data.Bits ((.&.), (.|.), xor, shift)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Text.Megaparsec
import Text.Megaparsec.Char
import Common.Parsers

import Debug.Trace (trace)
import Data.Foldable (foldl')
import Data.List (sortBy, intercalate, subsequences, sort)
--import Data.Ord (comparing, Down(Down))
import Data.Function (on)
import Text.Printf (printf, PrintfArg)

parseInputValue :: Parser (ByteString, Int)
parseInputValue = do
  name <- some alphaNumChar
  _ <- string ": "
  value <- parseInt
  _ <- eol
  return (X.pack name, value)

type BitOp = (Int -> Int -> Int)
type Operation = (BitOp, ByteString, ByteString, ByteString)

parseOperation :: Parser (BitOp, ByteString, ByteString, ByteString)
parseOperation = do
  n0 <- some alphaNumChar
  op <- choice [ (.&.) <$ string " AND "
               , (.|.) <$ string " OR "
               , xor <$ string " XOR " ]
  n1 <- some alphaNumChar
  _ <- string " -> "
  out <- some alphaNumChar
  _ <- eol
  return (op, X.pack n0, X.pack n1, X.pack out)

parseScheme :: Parser ([(ByteString, Int)], [Operation])
parseScheme = do
  inputs <- many parseInputValue
  _ <- eol
  ops <- many parseOperation
  return (inputs, ops)

parseProgram :: String -> ([(ByteString, Int)], [Operation])
parseProgram input = case runParser parseScheme "scheme" input of
                       Left e -> error (errorBundlePretty e)
                       Right x -> x

runValues :: Ord b => (Map b a, Map b (a -> a -> a, b, b)) -> b -> (Map b a, Map b (a -> a -> a, b, b))
runValues acc@(wires, eqs) var = case M.lookup var wires of
  Just v -> (M.insert var v wires, eqs)
  Nothing -> let (op, n0, n1) = fromJust $ M.lookup var eqs
                 acc0 = runValues acc n0
                 (wireUp, _) = runValues acc0 n1
                 v0 = fromJust $ M.lookup n0 wireUp
                 v1 = fromJust $ M.lookup n1 wireUp
                 res = op v0 v1
             in (M.insert var res wireUp, eqs)

findVars :: [(a, b, c, ByteString)] -> ByteString -> [ByteString]
findVars ops prefix = sort $ mapMaybe (\(_op, _n0, _n1, k) -> if prefix `X.isPrefixOf` k then Just k else Nothing) ops

findInputs :: [(ByteString, b)] -> ByteString -> [b]
findInputs inputs prefix = map snd $ sortBy (compare `on` fst) $ filter ((prefix `X.isPrefixOf`).fst) inputs

bitsToNumRaw :: [Int] -> Int
bitsToNumRaw bitsList = foldl' (\acc bit -> (shift acc 1) .|. bit) 0 $ reverse bitsList

bitsToNum :: Ord a => Map a Int -> [a] -> (Int, [Int])
bitsToNum wires vars =
  let bits = map (fromJust.(`M.lookup` wires)) vars
  in (bitsToNumRaw bits, bits)

numToBits :: [Int] -> Int -> [Int]
numToBits acc x = let b = x .&. 1
                      acc' = b:acc
                  in if x == 0
                     then reverse acc
                     else numToBits acc' (shift x (-1))

wrongBits :: ([Int], Int) -> [Int] -> [Int] -> [Int]
wrongBits (accBits, idx) a b
  | null a && null b = accBits
  | null a = wrongBits (idx:accBits, idx+1) a (tail b)
  | null b = wrongBits (idx:accBits, idx+1) (tail a) b
  | head a == head b = wrongBits (accBits, idx+1) (tail a) (tail b)
  | otherwise = wrongBits (idx:accBits, idx+1) (tail a) (tail b)

swapEq :: Ord k => Map k a -> k -> k -> Map k a
swapEq eqs k0 k1 = let eq0 = fromJust $ M.lookup k0 eqs
                       eq1 = fromJust $ M.lookup k1 eqs
                   in M.insert k0 eq1 $ M.insert k1 eq0 eqs

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

runSwap :: (Ord a, Show a) => [Int] -> [a] -> Map a Int -> Map a (Int -> Int -> Int, a, a) -> [a] -> [a] -> Bool
runSwap sbits zvars initialWires eqs origChanges changes = let a = head changes
                                                               b = changes !! 1
                                                               changesUp = drop 2 changes
                                                               eqsUp = swapEq eqs a b
                                                               (wires, _) = foldl' runValues (initialWires, eqsUp) zvars
                                                               (_z, zbits) = bitsToNum wires zvars
                                                               wrong = wrongBits ([], 0) sbits zbits
                                                               wrong' = trace("changes: " ++ show origChanges ++ ", wrong: " ++ show wrong ++ " -> " ++ show (length wrong) ++
                                                                         "\n" ++ intercalate "" (map show zbits) ++
                                                                         "\n" ++ intercalate "" (map show sbits)
                                                                        ) wrong
                                                           in if | not $ null changesUp -> runSwap sbits zvars initialWires eqsUp origChanges changesUp
                                                                 | null wrong' -> True
                                                                 | otherwise -> False

type Solution = Int
solve1 :: ByteString -> Solution
solve1 input = do
  let (inputs, ops) = parseProgram $ X.unpack input
  let zvars = findVars ops (X.pack "z")
  let outputs = foldl' (\acc (op, n0, n1, k) -> M.insert k (op, n0, n1) acc) M.empty ops
  let initialWires = M.fromList inputs
  let (wires, _) = foldl' runValues (initialWires, outputs) zvars
  fst $ bitsToNum wires zvars

changeNthElement :: Int -> (a -> a) -> [a] -> [a]
changeNthElement idx transform list
    | idx < 0   = list
    | otherwise = case splitAt idx list of
                    (front, element:back) -> front ++ transform element : back
                    _ -> list    -- if the list doesn't have an element at index idx

bitsToWires :: (PrintfArg b) => String -> [b] -> [(ByteString, b)]
bitsToWires prefix bits = map (\b -> (X.pack $ (printf "%s%02d" prefix b), b)) bits
--bitsToWires prefix bits = [(,) | X.pack $ (printf "%s%02d" prefix) <- ([0..] :: [Int]) bits

calcAndPrint ops ybits nth = do
  let xbits = changeNthElement nth (return 1) $ replicate (length ybits) 0
  let zvars = findVars ops (X.pack "z")
  let s = (bitsToNumRaw xbits) + (bitsToNumRaw ybits)
  let sbits = numToBits [] s

  let initialWires = M.fromList $ bitsToWires "x" xbits ++ bitsToWires "y" ybits
  let eqs = foldl' (\acc (op, n0, n1, k) -> M.insert k (op, n0, n1) acc) M.empty ops
  let (wires, _) = foldl' runValues (initialWires, eqs) zvars
  let (_z, zbits) = bitsToNum wires zvars
  let wrong' = trace("x:  " ++
                     intercalate "" (map show xbits) ++
                     "\ny:  " ++
                     intercalate "" (map show ybits) ++
                     "\ns:  " ++
                     intercalate "" (map show sbits) ++
                     "\nz:  " ++
                     intercalate "" (map show zbits)
                    ) 0
  wrong'

solve2 :: ByteString -> Solution
solve2 input = do
  let (inputs, ops) = parseProgram $ X.unpack input
  let xbitsOrig = findInputs inputs (X.pack "x")
  let xbits = changeNthElement 0 (return 1) $ replicate (length xbitsOrig) 0
  let ybits = findInputs inputs (X.pack "y")
  let zvars = findVars ops (X.pack "z")
  let s = (bitsToNumRaw xbits) + (bitsToNumRaw ybits)
  let sbits = numToBits [] s

  let eqs = foldl' (\acc (op, n0, n1, k) -> M.insert k (op, n0, n1) acc) M.empty ops
  let initialWires = M.fromList inputs
  let (wires, _) = foldl' runValues (initialWires, eqs) zvars
  let (_z, zbits) = bitsToNum wires zvars
  let wrong = wrongBits ([], 0) sbits zbits
  let ones = filter (\num -> (zbits !! num) == 1) wrong
  let zeros = filter (\num -> (zbits !! num) == 0) wrong
  let wrong' = trace("x:  " ++
                     intercalate "" (map show xbits) ++
                     "\ny:  " ++
                     intercalate "" (map show ybits) ++
                     "\ns:  " ++
                     intercalate "" (map show sbits) ++
                     "\nz:  " ++
                     intercalate "" (map show zbits) ++
                     "\n " ++
                     show wrong ++ "\nones: " ++ show ones ++ "\nzero: " ++ show zeros
                    ) wrong

  let keys = map (X.pack.(printf "z%02d")) wrong'

  let ret = map (calcAndPrint ops ybits) $ [0..(length xbitsOrig)]

  trace(show ret) 0
  -- let changes = take 1 $ mapMaybe (\testChanges -> let isGood = runSwap sbits zvars initialWires eqs testChanges testChanges
  --                                                  in if isGood then Just testChanges else Nothing
  --                                 ) $ combinations 8 keys
  -- trace("changes: " ++ show changes) 2
