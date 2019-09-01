import Data.Char
import Data.Map

-- Lexical Parser Begin

data Token
  = TokenInt Integer
  | TokenTrue | TokenFalse
  | TokenVar String
  | TokenPlus | TokenMinus | TokenMult | TokenDiv | TokenAnd | TokenOr | TokenGT | TokenLT | TokenAssign
  | TokenParOpn | TokenParCls | TokenBrkOpn | TokenBrkCls
  | TokenIf | TokenThen | TokenElse | TokenWhile | TokenDo
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('t':'r':'u':'e':cs) = TokenTrue : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = TokenFalse : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('a':'n':'d':cs) = TokenAnd : lexer cs
lexer ('o':'r':cs) = TokenOr : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer (':':'=':cs) = TokenAssign : lexer cs
lexer ('(':cs) = TokenParOpn : lexer cs
lexer (')':cs) = TokenParCls : lexer cs
lexer ('{':cs) = TokenBrkOpn : lexer cs
lexer ('}':cs) = TokenBrkCls : lexer cs
lexer ('i':'f':cs) = TokenIf : lexer cs
lexer ('t':'h':'e':'n':cs) = TokenThen : lexer cs
lexer ('e':'l':'s':'e':cs) = TokenElse : lexer cs
lexer ('w':'h':'i':'l':'e':cs) = TokenWhile : lexer cs
lexer ('d':'o':cs) = TokenDo : lexer cs
lexer all@(c:cs)
  | isDigit c = TokenInt (read . head $ tokens :: Integer) : (lexer . tail' $ tokens)
  | isAlpha c = (TokenVar . head $ tokens) : (lexer . tail' $ tokens)
  | otherwise = lexer cs
  where 
    tokens = words all
    tail' = unwords . tail

-- Lexical Parser End

-- Syntactic Parser Begin

type SymbTable = Map String Int

statment :: ([Token], SymbTable) -> SymbTable
statement ([], st) = st
statment (TokenVar var:TokenAssign:tokens, st) = statment . arithExp $ tokens
statment (TokenIf:tokens, st) = 

-- Syntactic Parser end

main :: IO ()
main = do
  input <- getContents
  print . lexer $ input