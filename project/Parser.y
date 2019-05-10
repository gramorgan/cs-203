
{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
    '('         { TokLPar      }
    ')'         { TokRPar      }
    'define'    { TokDefine    }
    'quote'     { TokQuote     }
    floatlit    { TokLitFloat $$ }
    boollit     { TokLitBool $$  }
    strlit      { TokLitStr $$   }
    lambda      { TokLambda      }
    ident       { TokIdent $$    }

%%

program    : action                                { [ $1 ]               }
           | action program                        {  $1 : $2             }

action     : assignment                            { AssignAction $1      }
           | expr                                  { ExprAction $1        }

assignment : '(' 'define' ident expr ')'           { LispAssignment $3 $4 }

expr       : '(' lambda '(' paramlist ')' expr ')' { LambdaExpr $4 $6     }
           | '(' expr arglist ')'                  { CallExpr   $2 $3     }
           | ident                                 { IdentExpr  $1        }
           | floatlit                              { FloatExpr  $1        }
           | boollit                               { BoolExpr   $1        }
           | strlit                                { StrExpr    $1        }

paramlist  : ident                                 { [ $1 ]               }
           | ident paramlist                       { $1 : $2              }

arglist    : expr                                  { [ $1 ]               }
           | expr arglist                          { $1 : $2              }

{

parseError = error "parse error"

type LispIdent = String

data LispAction
    = ExprAction LispExpr
    | AssignAction LispAssignment
    deriving Show

data LispAssignment = LispAssignment LispIdent LispExpr
    deriving Show

data LispExpr
    = LambdaExpr [LispIdent]  LispExpr
    | CallExpr    LispExpr   [LispExpr]
    | IdentExpr   LispIdent
    | FloatExpr   Float
    | BoolExpr    Bool
    | StrExpr     String
    deriving Show

}

