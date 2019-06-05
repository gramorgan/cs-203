
-- scheme tokenizer made using alex

{
module Lexer where
}

%wrapper "basic"

$identchars = ~[$white \( \) \']

tokens :-
    $white+         ;
    ";".*           ;
    \(              { \s -> TokLPar              }
    \)              { \s -> TokRPar              }
    \-?[0-9 \.]+    { \s -> TokLitFloat $ read s }
    if              { \s -> TokIf                }
    define          { \s -> TokDefine            }
    quote           { \s -> TokQuote             }
    \'              { \s -> TokTick              }
    lambda          { \s -> TokLambda            }
    if              { \s -> TokIf                }
    let             { \s -> TokLet               }
    \#t             { \s -> TokLitBool True      }
    \#f             { \s -> TokLitBool False     }
    \"~\"*\"        { \s -> TokLitStr s          }
    $identchars+    { \s -> TokIdent s           }

{

data Token
    = TokLPar
    | TokRPar
    | TokLitFloat Float
    | TokLitBool  Bool
    | TokLitStr   String
    | TokIf
    | TokLet
    | TokDefine
    | TokQuote
    | TokTick
    | TokLambda
    | TokIdent    String
    deriving Show

}

