
{
module Parser
( parser
, LispExpr (..)
) where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
    '('         { TokLPar        }
    ')'         { TokRPar        }
    'define'    { TokDefine      }
    'quote'     { TokQuote       }
    'lambda'    { TokLambda      }
    'if'        { TokIf          }
    'let'       { TokLet         }
    floatlit    { TokLitFloat $$ }
    boollit     { TokLitBool $$  }
    strlit      { TokLitStr $$   }
    ident       { TokIdent $$    }

%%

program    : expr                                    { [ $1 ]               }
           | expr program                            {  $1 : $2             }

expr       : '(' 'lambda' '(' paramlist ')' expr ')' { LambdaExpr $4 $6     }
           | '(' 'define' ident expr ')'             { DefineExpr $3 $4     }
           | '(' expr arglist ')'                    { CallExpr   $2 $3     }
           | '(' 'quote' quoteexpr ')'               { QuoteExpr  $3        }
           | '(' 'if' expr expr expr ')'             { IfExpr     $3 $4 $5  }
           | '(' 'let' '(' letlist ')' expr ')'      { LetExpr    $4 $6     }
           | ident                                   { IdentExpr  $1        }
           | floatlit                                { FloatExpr  $1        }
           | boollit                                 { BoolExpr   $1        }
           | strlit                                  { StrExpr    $1        }

paramlist  : ident                                   { [ $1 ]               }
           | ident paramlist                         { $1 : $2              }

arglist    : expr                                    { [ $1 ]               }
           | expr arglist                            { $1 : $2              }

quoteexpr  : ident                                   { IdentExpr  $1        }
           | floatlit                                { FloatExpr  $1        }
           | boollit                                 { BoolExpr   $1        }
           | strlit                                  { StrExpr    $1        }
           | '(' quotelist ')'                       { ListExpr   $2        }

quotelist  : {- empty -}                             { []                   }
           | quoteexpr quotelist                     { $1 : $2              }

letlist    : letexpr                                 { [ $1 ]               }
           | letexpr letlist                         { $1 : $2              }

letexpr    : '(' ident expr ')'                      { ($2, $3)             }
{

parseError = error "parse error"

data LispExpr
    = LambdaExpr [String]   LispExpr
    | DefineExpr  String    LispExpr
    | CallExpr    LispExpr [LispExpr]
    | IfExpr      LispExpr  LispExpr  LispExpr
    | LetExpr    [(String, LispExpr)] LispExpr
    | IdentExpr   String 
    | FloatExpr     Float
    | BoolExpr    Bool
    | StrExpr     String
    | ListExpr   [LispExpr]
    | QuoteExpr   LispExpr
    deriving Show

--instance Show LispExpr where
--    show (IdentExpr s) = s
--    show (FloatExpr n) = show n
--    show (BoolExpr  b) = if b then "#t" else "#f"
--    show (StrExpr   s) = s
--    show (ListExpr  l) = "(" ++ (unwords $ map show l) ++ ")"
--    show _             = "unimplemented"

}

