-- simple lexer and parser made using happy
-- mostly taken from https://www.haskell.org/happy/doc/html/sec-using.html
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
    int        { TokenInt $$ }
    var        { TokenVar $$ }
    true       { TokenTrue   }
    false      { TokenFalse  }
    skip       { TokenSkip   }
    if         { TokenIf     }
    then       { TokenThen   }
    else       { TokenElse   }
    while      { TokenWhile  }
    do         { TokenDo     }
    '+'        { TokenSum    }
    '-'        { TokenDiff   }
    '*'        { TokenMul    }
    '('        { TokenLPar   }
    ')'        { TokenRPar   }
    '='        { TokenEq     }
    '<'        { TokenLt     }
    '&'        { TokenAnd    }
    '|'        { TokenOr     }
    '~'        { TokenNot    }
    ';'        { TokenComp   }
    ':='       { TokenAss    }

%left ';'
%nonassoc do else
%left '&' '|'
%right '~'
%nonassoc '<' ':='
%left '+' '-'
%left '*'

%%

Comm   : skip               { SkipComm        }
       | var ':=' AExp      { AssComm $1 $3   }
       | Comm ';' Comm      { CompComm $1 $3  }
       | if BExp then Comm else Comm
                            { IfComm $2 $4 $6 }
       | while BExp do Comm { WhileComm $2 $4 }
       | '(' Comm ')'       { $2              }

BExp   : true               { TrueBExp        }
       | false              { FalseBExp       }
       | AExp '=' AExp      { EqBExp  $1 $3   }
       | AExp '<' AExp      { LtBExp  $1 $3   }
       | '~' BExp           { NotBExp $2      }
       | BExp '&' BExp      { AndBExp $1 $3   }
       | BExp '|' BExp      { OrBExp  $1 $3   }
       | '(' BExp ')'       { $2              }

AExp   : int                { IntAExp  $1     }
       | var                { VarAExp  $1     }
       | AExp '+' AExp      { SumAExp  $1 $3  }
       | AExp '-' AExp      { DiffAExp $1 $3  }
       | AExp '*' AExp      { MulAExp  $1 $3  }
       | '(' AExp ')'       { $2              }

{

parseError :: [WhileToken] -> a
parseError _ = error "Parse error"

}
