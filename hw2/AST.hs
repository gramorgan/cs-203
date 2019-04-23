
-- types for While AST
module AST where

-- variable names are just strings
type WhileVar = String

-- arithmetic expressions
data AExp
    = IntAExp   Int
    | VarAExp   WhileVar
    | SumAExp   AExp AExp
    | DiffAExp  AExp AExp
    | MulAExp   AExp AExp

-- boolean expressions
data BExp
    = TrueBExp
    | FalseBExp
    | EqBExp    AExp AExp
    | LtBExp    AExp AExp
    | NotBExp   BExp
    | AndBExp   BExp BExp
    | OrBExp    BExp BExp

-- command expressions
data Comm
    = SkipComm
    | AssComm   WhileVar  AExp
    | CompComm  Comm Comm
    | IfComm    BExp Comm Comm
    | WhileComm BExp Comm

