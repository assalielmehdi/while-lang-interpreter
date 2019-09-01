import Data.Map
import Data.Maybe
import Text.Read

data BoolExp
  = BoolLit BoolVal
  | LogicOp BoolVal BoolOp BoolVal
  | ArithComp ArithExp RelOp ArithExp
  | ParBoolExp BoolExp
  deriving Show

data BoolVal
  = BoolTrue
  | BoolFalse
  deriving Show

data BoolOp
  = BoolAnd
  | BoolOr
  deriving Show

data RelOp
  = RelGT
  | RelLT
  deriving Show

data ArithExp = ArithExp Term ArithExpAux deriving Show

data ArithExpAux
  = ArithPlus Term ArithExpAux
  | ArithMinus Term ArithExpAux
  | StopArith
  deriving Show

data Term = Term Factor TermAux deriving Show

data TermAux
  = TermMult Factor TermAux
  | TermDiv Factor TermAux
  | StopTerm
  deriving Show

data Factor
  = FactorVar String
  | FactorInt Integer
  | FactorPar ArithExp
  deriving Show

type SymbTable = Map String Integer

stmt :: [String] -> SymbTable -> SymbTable
stmt [] st = st
stmt (tk1:tk2:tks) st
  | tk1 == "if" = st
  | tk1 == "while" = st
  | otherwise = (\(val, tks) -> stmt tks (insert tk1 (evalArithExp val st) st)) . arithExp tks $ st

arithExp :: [String] -> SymbTable -> (ArithExp, [String])
arithExp tks st = do
  let (t, tks1) = term tks st
  let (a, tks2) = arithExpAux tks1 st
  (ArithExp t a, tks2)

arithExpAux :: [String] -> SymbTable -> (ArithExpAux, [String])
arithExpAux [] st = (StopArith, [])
arithExpAux all@(tk1:tks) st
  | tk1 == "+" = do
    let (t, tks1) = term tks st
    let (a, tks2) = arithExpAux tks1 st
    (ArithPlus t a, tks2)
  | tk1 == "-" = do
    let (t, tks1) = term tks st
    let (a, tks2) = arithExpAux tks1 st
    (ArithMinus t a, tks2)
  | otherwise = (StopArith, all)

term :: [String] -> SymbTable -> (Term, [String])
term tks st = do
  let (f, tks1) = factor tks st
  let (t, tks2) = termAux tks1 st
  (Term f t, tks2)

termAux :: [String] -> SymbTable -> (TermAux, [String])
termAux [] st = (StopTerm, [])
termAux all@(tk1:tks) st
  | tk1 == "*" = do
    let (f, tks1) = factor tks st
    let (t, tks2) = termAux tks1 st
    (TermMult f t, tks2)
  | tk1 == "/" = do
    let (f, tks1) = factor tks st
    let (t, tks2) = termAux tks1 st
    (TermDiv f t, tks2)
  | otherwise = (StopTerm, all)

factor :: [String] -> SymbTable -> (Factor, [String])
factor (tk1:tks) st
  | tk1 == "(" = do
    let (a, _:tks1) = arithExp tks st
    (FactorPar a, tks1)
  | isNothing maybeNumber = (FactorVar tk1, tks)
  | otherwise = (FactorInt (read tk1 :: Integer), tks)
  where maybeNumber = readMaybe tk1 :: Maybe Integer

evalArithExp :: ArithExp -> SymbTable -> Integer
evalArithExp (ArithExp t a) st = evalArithExpAux (evalTerm t st) a st

evalArithExpAux :: Integer -> ArithExpAux -> SymbTable -> Integer
evalArithExpAux left StopArith _ = left
evalArithExpAux left (ArithPlus t a) st = evalArithExpAux (left + evalTerm t st) a st
evalArithExpAux left (ArithMinus t a) st = evalArithExpAux (left - evalTerm t st) a st

evalTerm :: Term -> SymbTable -> Integer
evalTerm (Term f t) st = evalTermAux (evalFactor f st) t st

evalTermAux :: Integer -> TermAux -> SymbTable -> Integer
evalTermAux left StopTerm _ = left
evalTermAux left (TermMult f t) st = evalTermAux (left * evalFactor f st) t st
evalTermAux left (TermDiv f t) st = evalTermAux (left `div` evalFactor f st) t st

evalFactor :: Factor -> SymbTable -> Integer
evalFactor (FactorVar s) st = st ! s
evalFactor (FactorInt n) _ = n
evalFactor (FactorPar a) st = evalArithExp a st

main :: IO ()
main = do
  input <- getContents
  print . stmt (words input) $ fromList []