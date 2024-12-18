module Day17 (solve1, solve2) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Bits (xor)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Debug.Trace (trace)

import Common.Parsers (Parser, parseInt)

data Registers = Registers
  { regA :: Int
  , regB :: Int
  , regC :: Int
  }
  deriving (Eq, Show)

pSingleRegister :: Parser Int
pSingleRegister = do
  _ <- string "Register "
  _ <- char 'A' <|> char 'B' <|> char 'C'
  _ <- string ": "
  reg <- parseInt
  _ <- optional eol
  return reg

pRegisters :: Parser Registers
pRegisters = do
  a <- pSingleRegister
  b <- pSingleRegister
  c <- pSingleRegister
  return (Registers a b c)

pProgram :: Parser [Int]
pProgram = do
  _ <- string "Program: "
  sepBy1 parseInt (char ',')

pWholeProgram :: Parser (Registers, [Int])
pWholeProgram = do
  regs <- pRegisters
  _ <- eol
  ops <- pProgram
  return (regs, ops)

parseProgram :: String -> (Registers, [Int])
parseProgram input = case runParser pWholeProgram "program" input of
                       Left e -> error (errorBundlePretty e)
                       Right x -> x

data Operand = LiteralOperand Int
             | ComboOperand Int
             | IgnoredOperand Int
             deriving (Show, Eq)

type IP = Int
type Op = (IP, [Int], Registers)

value :: Registers -> Operand -> Int
value (Registers a b c) op@(ComboOperand rawOp) =
  case rawOp of
    0 -> rawOp
    1 -> rawOp
    2 -> rawOp
    3 -> rawOp
    4 -> a
    5 -> b
    6 -> c
    7 -> error "reserved"
    _ -> error("unsupported operation " ++ show op)

value _ (LiteralOperand x) = x
value _ (IgnoredOperand x) = x

constStep :: Int
constStep = 2

division :: Registers -> Operand -> Int
division regs@(Registers a _b _c) op = let nom = a
                                           den = 2 ^ (value regs op)
                                       in nom `div` den


adv :: Op -> Operand -> Op
adv (ip, ins, regs@(Registers _a b c)) op = let x = division regs op
                                            in (ip + constStep, ins, Registers x b c)

bxl :: Op -> Operand -> Op
bxl (ip, ins, regs@(Registers a b c)) op = (ip + constStep, ins, Registers a (b `xor` (value regs op)) c)

bst :: Op -> Operand -> Op
bst (ip, ins, regs@(Registers a _b c)) op = let x = (value regs op) `mod` 8
                                            in (ip + constStep, ins, Registers a x c)

jnz :: Op -> Operand -> Op
jnz (ip, ins, regs@(Registers a _b _c)) op
  | a == 0 = (ip, ins, regs)
  | otherwise = (value regs op, ins, regs)

bxc :: Op -> Operand -> Op
bxc (ip, ins, _regs@(Registers a b c)) _op = (ip + constStep, ins, Registers a (b `xor` c) c)

out :: Op -> Operand -> Op
out (ip, ins, regs@(Registers _a _b _c)) op = let x = (value regs op) `mod` 8
                                              in (ip + constStep, ins++[x], regs)

bdv :: Op -> Operand -> Op
bdv (ip, ins, regs@(Registers a _b c)) op = let x = division regs op
                                            in (ip + constStep, ins, Registers a x c)

cdv :: Op -> Operand -> Op
cdv (ip, ins, regs@(Registers a b _c)) op = let x = division regs op
                                            in (ip + constStep, ins, Registers a b x)

operations :: Map Int (Op -> Operand -> Op, Int -> Operand)
operations = Map.fromList [
  (0, (adv, ComboOperand)),
  (1, (bxl, LiteralOperand)),
  (2, (bst, ComboOperand)),
  (3, (jnz, LiteralOperand)),
  (4, (bxc, IgnoredOperand)),
  (5, (out, ComboOperand)),
  (6, (bdv, ComboOperand)),
  (7, (cdv, ComboOperand))
  ]

runOps :: Op -> Vector Int -> [Int] -> Bool -> Op
runOps op@(ip, _ins, _regs) allOps allOpsL checkOutput
  | ip >= V.length allOps = op
  | otherwise = let opIdx = allOps!ip
                    (func, constructor) = case Map.lookup opIdx operations of
                                            Nothing -> error ("bug: opIdx: " ++ show opIdx ++ ", op: " ++ show op)
                                            Just f -> f
                    d = constructor (allOps!(ip + 1))
                    nextOp@(nip, nout, _nregs) = func op d
                    retSt = if nip /= ip
                            then runOps nextOp allOps allOpsL checkOutput
                            else nextOp
                in if checkOutput && opIdx == 5
                   then if nout `isPrefixOf` allOpsL
                        then retSt
                        else nextOp
                   else retSt

searchForPrefix :: Registers -> Vector Int -> [Int] -> (Registers, [Int], Bool)
searchForPrefix regs _opsV [] = (regs, [], True)
searchForPrefix regs@(Registers a b c) opsV opsL@(prefix:opsRest) = do
  let outR = filter (\(_, nout) -> take 1 nout == [prefix]) $
        map (\i -> let aReg = (a*8 + i)
                       (_, nout, _) = runOps (0, [], Registers aReg b c) opsV opsL True
                   in (Registers aReg b c, nout)
            ) [0..7]

  if null outR
    then (regs, [], False)
    else let ret = dropWhile (\(_, _, completed) -> not completed) $
                   map (\(regsF, _) -> searchForPrefix regsF opsV opsRest) outR
         in if null ret
            then (regs, [], False)
            else head ret

type Solution = Int

solve1 :: String -> Solution
solve1 input = do
  let (regs, opsL) = parseProgram input
  let opsV = V.fromList opsL
  let (_, output, _) = runOps (0, [], regs) opsV opsL False
  trace(show output) 1

solve2 :: String -> Solution
solve2 input = do
  let (Registers _a b c, opsL) = parseProgram input
  let opsV = V.fromList opsL
  let (Registers na _nb _nc, _, _completed) = searchForPrefix (Registers 0 b c) opsV (reverse opsL)
  na
