-- simple lexer and parser made using happy
-- mostly taken from https://www.haskell.org/happy/doc/html/sec-using.html
{
module Parse 
( lexer
, parser
, Exp (..)
) where

import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    float  { TokenFloat $$ }
    '+'    { TokenPlus }
    '*'    { TokenTimes }
    '-'    { TokenMinus }
    '/'    { TokenDiv }
    '('    { TokenLPar }
    ')'    { TokenRPar }

%%

Exp    : Exp '+' Term     { SumExp $1 $3 }
       | Exp '-' Term     { DiffExp $1 $3 }
       | Term             { $1 }

Term   : Term '*' Factor  { MulExp $1 $3 }
       | Term '/' Factor  { DivExp $1 $3 }
       | Factor           { $1 }

Factor : float            { FloatExp $1 }
       | '-' Factor       { InvExp $2 }
       | '(' Exp ')'      { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- represents an AST for an ARITH expression
data Exp
    = FloatExp Float
    | SumExp Exp Exp
    | DiffExp Exp Exp
    | MulExp Exp Exp
    | DivExp Exp Exp
    | InvExp Exp
    deriving Show

-- tokens for use with the lexer and parser
data Token
    = TokenFloat Float
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenLPar
    | TokenRPar
    
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = lexer xs
    | digitOrDot x = lexNum (x:xs)
lexer ('+':xs) = TokenPlus  : lexer xs
lexer ('-':xs) = TokenMinus : lexer xs
lexer ('*':xs) = TokenTimes : lexer xs
lexer ('(':xs) = TokenLPar  : lexer xs
lexer (')':xs) = TokenRPar  : lexer xs

lexNum :: String -> [Token]
lexNum xs = TokenFloat (read num) : lexer rest
    where (num, rest) = span digitOrDot xs

digitOrDot :: Char -> Bool
digitOrDot a = a == '.' || isDigit a

}
