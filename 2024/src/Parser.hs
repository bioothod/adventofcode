module Parser
  ( Parser
  , parseInt
) where

import Data.Maybe ( fromMaybe )

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

signParser :: Parser Int
signParser = choice
  [ -1 <$ char '-'
  , 1 <$ char '+' ]

parseInt :: Parser Int
parseInt = do
  xMult <- optional signParser
  x <- some digitChar
  return (read x * fromMaybe 1 xMult)
