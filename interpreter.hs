import Data.Map
import Data.Maybe
import Text.Read

-- Grammar

data Stmt 
  = AssignStmt String ArithExp StmtAux
  | IfStmt BoolExp Stmt Stmt StmtAux
  | WhileStmt BoolExp Stmt StmtAux
  deriving Show

data StmtAux
  = NextStmt Stmt
  | StopStmt
  deriving Show

data BoolExp = BoolExp BoolTerm BoolExpAux deriving Show

data BoolExpAux
  = BoolExpOr BoolTerm BoolExpAux
  | StopBoolExp
  deriving Show

data BoolTerm = BoolTerm BoolFactor BoolTermAux deriving Show

data BoolTermAux
  = BoolTermAnd BoolFactor BoolTermAux
  | StopBoolTerm
  deriving Show

data BoolFactor
  = BoolVal Bool
  | BoolFactorPar BoolExp
  | BoolFactorComp ArithComp
  deriving Show

data ArithComp = ArithComp ArithExp ArithCompAux deriving Show

data ArithCompAux
  = ArithCompGT ArithExp
  | ArithCompLT ArithExp
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

-- Types

type SymbTable = Map String Integer

-- Statement functions

stmt :: [String] -> (Stmt, [String])
stmt ("if":tks) = do
  let (b, "then":"{":tks') = boolExp tks
  let (sTrue, "}":"else":"{":tks) = stmt tks'
  let (sFalse, "}":tks') = stmt tks
  let (next, tks) = stmtAux tks'
  (IfStmt b sTrue sFalse next, tks)
stmt ("while":tks) = do
  let (b, "do":"{":tks') = boolExp tks
  let (s, "}":tks) = stmt tks'
  let (next, tks') = stmtAux tks
  (WhileStmt b s next, tks')
stmt (var:":=":tks) = do
  let (a, tks') = arithExp tks
  let (next, tks) = stmtAux tks'
  (AssignStmt var a next, tks)

stmtAux :: [String] -> (StmtAux, [String])
stmtAux (";":tks) = do
  let (s, tks') = stmt tks
  (NextStmt s, tks')
stmtAux tks = (StopStmt, tks)

evalStmt :: Stmt -> SymbTable -> SymbTable
evalStmt (AssignStmt s a nxt) st = do
  let val = evalArithExp a st
  evalStmtAux nxt (insert s val st)
evalStmt (IfStmt b sTrue sFalse nxt) st = do
  let pred = evalBoolExp b st
  let st' = if pred then evalStmt sTrue st else evalStmt sFalse st
  evalStmtAux nxt st'

evalStmtAux :: StmtAux -> SymbTable -> SymbTable
evalStmtAux StopStmt st = st
evalStmtAux (NextStmt s) st = evalStmt s st

-- Boolean expression functions

boolExp :: [String] -> (BoolExp, [String])
boolExp tks = do
  let (t, tks') = boolTerm tks
  let (b, tks) = boolExpAux tks'
  (BoolExp t b, tks)

boolExpAux :: [String] -> (BoolExpAux, [String])
boolExpAux ("or":tks) = do
  let (t, tks') = boolTerm tks
  let (b, tks) = boolExpAux tks'
  (BoolExpOr t b, tks)
boolExpAux tks = (StopBoolExp, tks)

boolTerm :: [String] -> (BoolTerm, [String])
boolTerm tks = do
  let (f, tks') = boolFactor tks
  let (t, tks) = boolTermAux tks'
  (BoolTerm f t, tks)

boolTermAux :: [String] -> (BoolTermAux, [String])
boolTermAux ("and":tks) = do
  let (f, tks') = boolFactor tks
  let (t, tks) = boolTermAux tks'
  (BoolTermAnd f t, tks)
boolTermAux tks = (StopBoolTerm, tks)

boolFactor :: [String] -> (BoolFactor, [String])
boolFactor ("true":tks) = (BoolVal True, tks)
boolFactor ("false":tks) = (BoolVal False, tks)
boolFactor ("(":tks) = do
  let (e, ")":tks') = boolExp tks
  (BoolFactorPar e, tks')
boolFactor tks = do
  let (c, tks') = arithComp tks
  (BoolFactorComp c, tks')

arithComp :: [String] -> (ArithComp, [String])
arithComp tks = do
  let (e, tks') = arithExp tks
  let (a, tks) = arithCompAux tks'
  (ArithComp e a, tks)

arithCompAux :: [String] -> (ArithCompAux, [String])
arithCompAux (">":tks) = do
  let (e, tks') = arithExp tks
  (ArithCompGT e, tks')
arithCompAux ("<":tks) = do
  let (e, tks') = arithExp tks
  (ArithCompLT e, tks')

evalBoolExp :: BoolExp -> SymbTable -> Bool
evalBoolExp _ _ = False

-- Arithmetic expression functions

arithExp :: [String] -> (ArithExp, [String])
arithExp tks = do
  let (t, tks') = term tks
  let (a, tks) = arithExpAux tks'
  (ArithExp t a, tks)

arithExpAux :: [String] -> (ArithExpAux, [String])
arithExpAux ("+":tks) = do
  let (t, tks') = term tks
  let (a, tks) = arithExpAux tks'
  (ArithPlus t a, tks)
arithExpAux ("-":tks) = do
  let (t, tks') = term tks
  let (a, tks) = arithExpAux tks'
  (ArithMinus t a, tks)
arithExpAux tks = (StopArith, tks)

term :: [String] -> (Term, [String])
term tks = do
  let (f, tks') = factor tks
  let (t, tks) = termAux tks'
  (Term f t, tks)

termAux :: [String] -> (TermAux, [String])
termAux [] = (StopTerm, [])
termAux ("*":tks) = do
  let (f, tks') = factor tks
  let (t, tks) = termAux tks'
  (TermMult f t, tks)
termAux ("/":tks) = do
  let (f, tks') = factor tks
  let (t, tks) = termAux tks'
  (TermDiv f t, tks)
termAux tks = (StopTerm, tks)

factor :: [String] -> (Factor, [String])
factor ("(":tks) = do
  let (a, ")":tks') = arithExp tks
  (FactorPar a, tks')
factor (tk:tks)
  | isNothing maybeNumber = (FactorVar tk, tks)
  | otherwise = (FactorInt (read tk :: Integer), tks)
  where maybeNumber = readMaybe tk :: Maybe Integer

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

-- Main

main :: IO ()
main = do
  input <- getContents
  -- let prgm = fst . stmt . words $ input
  -- print . evalStmt prgm $ fromList []
  print . stmt . words $ input