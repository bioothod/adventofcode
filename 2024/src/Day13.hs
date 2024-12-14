{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day13 (solve1, solve2) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (fromMaybe, catMaybes)
--import Math.LinearEquationSolver (solveIntegerLinearEqs)

priceA :: Int
priceB :: Int
priceA = 3
priceB = 1

data Button = Button
  { price :: Int
  , stepX :: Int
  , stepY :: Int
  } deriving (Eq, Show)

data Coord = Coord
  { coordX :: Int
  , coordY :: Int
  } deriving (Eq, Show)

data Machine = Machine
  { buttonA :: Button
  , buttonB :: Button
  , location :: Coord
  } deriving (Eq, Show)

type Parser = Parsec Void String

parseSingleCoord :: Parser Int
parseSingleCoord = do
  step <- some digitChar
  return (read step)

buttonTypeParser :: Parser Int
buttonTypeParser = choice
  [ priceA <$ char 'A'
  , priceB <$ char 'B' ]

signParser :: Parser Int
signParser = choice
  [ -1 <$ char '-'
  , 1 <$ char '+' ]


buttonParser :: Parser Button
buttonParser = do
  _ <- string "Button "
  price <- buttonTypeParser
  _ <- string ": "
  _ <- string "X"
  xMult <- optional signParser
  xStep <- parseSingleCoord
  _ <- string ", "
  _ <- string "Y"
  yMult <- optional signParser
  yStep <- parseSingleCoord
  _ <- char '\n'
  return (Button price (xStep * fromMaybe 1 xMult) (yStep * fromMaybe 1 yMult))

locationParser :: Parser Coord
locationParser = do
  _ <- string "Prize: "
  _ <- string "X="
  x <- some digitChar
  _ <- string ", "
  _ <- string "Y="
  y <- some digitChar
  _ <- char '\n'
  return (Coord (read x) (read y))

machineParser :: Parser Machine
machineParser = do
  bA <- buttonParser
  bB <- buttonParser
  loc <- locationParser
  _ <- optional (char '\n')
  return (Machine bA bB loc)

allMachines :: Parser [Machine]
allMachines = many machineParser

isInt :: Rational -> Bool
isInt x = x == fromInteger (round x)

solveManually :: Int -> Machine -> Maybe Int
solveManually coordOffset (Machine buttonA buttonB coord) = do
  let x = toRational (coordOffset + coordX coord)
  let y = toRational (coordOffset + coordY coord)
  let aX = toRational (stepX buttonA)
  let aY = toRational (stepY buttonA)
  let bX = toRational (stepX buttonB)
  let bY = toRational (stepY buttonB)

  let nA = (y*bX - x*bY) / (aY*bX - aX*bY)
  let nB = (x - aX*nA) / bX

  if (isInt nA) && (isInt nB)
    then Just ((round nA)*priceA + (round nB)*priceB)
    else Nothing

type Solution = Int

solve1 :: String -> Solution
solve1 input = do
  let machines = case runParser allMachines "name" input of
        Left e -> error (errorBundlePretty e)
        Right x -> x

  let score = sum $ catMaybes $ map (solveManually 0) machines
  score

solve2 :: String -> Solution
solve2 input = do
  let machines = case runParser allMachines "name" input of
        Left e -> error (errorBundlePretty e)
        Right x -> x

  let score = sum $ catMaybes $ map (solveManually 10000000000000) machines
  score
