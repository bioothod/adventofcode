module Day03 (solve1, solve2) where

import Data.Void
import Data.Either (rights)
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Solution = Integer

data Instruction = Mul Int Int
                 | Do
                 | Dont
  deriving (Show)

type Parser = Parsec Void String

parseMul :: Parser Instruction
parseMul = do
  _ <- chunk "mul"
  _ <- char '('
  left <- some digitChar
  _ <- char ','
  right <- some digitChar
  _ <- char ')'
  return $ Mul (read left) (read right)

parseDo :: Parser Instruction
parseDo = do
  _ <- chunk "do()"
  return Do

parseDont :: Parser Instruction
parseDont = do
  _ <- chunk "don't()"
  return Dont

parseEof :: Parser Instruction
parseEof = do
  _ <- eof
  return Dont

parseEnabledBlock :: Parser String
parseEnabledBlock = do
  _ <- parseDo
  ret <- fst <$> anyTill (parseDont <|> parseEof)
  return ret

dereference_instruction :: Instruction -> Int
dereference_instruction (Mul a b) = a * b
dereference_instruction _ = error "can not dereference"

parse_lines :: [String] -> Integer
parse_lines input = do
  let right_instructions = map (map snd) . map rights $ map (splitCap (match parseMul)) input
  let instructions = map (map dereference_instruction) right_instructions
  toInteger . foldl (+) 0 $ map (foldl (+) 0) instructions

solve1 :: String -> Solution
solve1 input = do
  parse_lines $ lines input

solve2 :: String -> Solution
solve2 input = do
  parse_lines . map snd . rights . splitCap (match parseEnabledBlock) $ "do()" ++ input ++ "don't()"
