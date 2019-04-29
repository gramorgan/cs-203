-- simple parser made using happy
{
module Parse 
( parser
) where

import Tokens
import AST
import Data.Char
}

%name parser
%tokentype { WhileToken }
%error { parseError }

%token
    intLit     { TokenLitInt  $$ }
    boolLit    { TokenLitBool $$ }
    var        { TokenVar $$     }
    int        { TokenInt        }
    bool       { TokenBool       }
    skip       { TokenSkip       }
    if         { TokenIf         }
    then       { TokenThen       }
    else       { TokenElse       }
    while      { TokenWhile      }
    do         { TokenDo         }
    '+'        { TokenSum        }
    '-'        { TokenDiff       }
    '*'        { TokenMul        }
    '('        { TokenLPar       }
    ')'        { TokenRPar       }
    '='        { TokenEq         }
    '<'        { TokenLt         }
    '&'        { TokenAnd        }
    '|'        { TokenOr         }
    '~'        { TokenNot        }
    ';'        { TokenComp       }
    ':='       { TokenAss        }

%left ';'
%nonassoc do else
%left '&' '|'
%right '~'
%nonassoc '<'
%left '+' '-'
%left '*'

%%

Comm   : skip               { SkipComm         }
       | int  var ':=' AExp { AssIntComm  $2 $4   }
       | bool var ':=' BExp { AssBoolComm  $2 $4   }
       | Comm ';' Comm      { CompComm $1 $3   }
       | if BExp then Comm else Comm
                            { IfComm $2 $4 $6  }
       | while BExp do Comm { WhileComm $2 $4  }
       | '(' Comm ')'       { $2               }

BExp   : boolLit            { BoolBExp $1      }
       | var                { VarBExp  $1      }
       | AExp '=' AExp      { EqBExp   $1 $3   }
       | AExp '<' AExp      { LtBExp   $1 $3   }
       | '~' BExp           { NotBExp  $2      }
       | BExp '&' BExp      { AndBExp  $1 $3   }
       | BExp '|' BExp      { OrBExp   $1 $3   }
       | '(' BExp ')'       { $2               }

AExp   : intLit             { IntAExp  $1      }
       | var                { VarAExp  $1      }
       | AExp '+' AExp      { SumAExp  $1 $3   }
       | AExp '-' AExp      { DiffAExp $1 $3   }
       | AExp '*' AExp      { MulAExp  $1 $3   }
       | '(' AExp ')'       { $2               }

{

parseError :: [WhileToken] -> a
parseError _ = error "Parse error"

}
