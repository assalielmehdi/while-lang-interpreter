import Data.Map
import Data.Maybe
import Text.Read

newtype BoolExp
  = BoolLit BoolVal
  -- | LogicOp BoolVal BoolOp BoolVal
  -- | ArithComp ArithExp RelOp ArithExp
  -- | ParBoolExp BoolExp
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

data Stmt 
  = AssignStmt String ArithExp StmtAux
  | IfStmt BoolExp Stmt Stmt StmtAux
  | WhileStmt BoolExp Stmt StmtAux
  deriving Show

data StmtAux
  = NextStmt Stmt
  | StopStmt
  deriving Show

type SymbTable = Map String Integer

-- Statement

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

--

-- Arithmetic Expression

arithExp :: [String] -> (ArithExp, [String])
arithExp tks = do
  let (t, tks') = term tks
  let (a, tks) = arithExpAux tks'
  (ArithExp t a, tks)

arithExpAux :: [String] -> (ArithExpAux, [String])
arithExpAux [] = (StopArith, [])
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

--

-- Boolean Expression

boolExp :: [String] -> (BoolExp, [String])
boolExp [] = (BoolLit BoolFalse, [])
boolExp (tk:tks)
  | tk == "true" = (BoolLit BoolTrue, tks)
  | otherwise = (BoolLit BoolFalse, tks)

evalBoolExp :: BoolExp -> SymbTable -> Bool
evalBoolExp (BoolLit BoolTrue) _ = True
evalBoolExp (BoolLit BoolFalse) _ = False

--

main :: IO ()
main = do
  input <- getContents
  -- let prgm = fst . stmt . words $ input
  -- print . evalStmt prgm $ fromList []
  print . stmt . words $ input