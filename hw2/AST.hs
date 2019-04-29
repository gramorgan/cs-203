
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
    deriving Show

-- boolean expressions
data BExp
    = BoolBExp  Bool
    | VarBExp   WhileVar
    | EqBExp    AExp AExp
    | LtBExp    AExp AExp
    | NotBExp   BExp
    | AndBExp   BExp BExp
    | OrBExp    BExp BExp
    deriving Show

-- command expressions
data Comm
    = SkipComm
    | AssIntComm   WhileVar  AExp
    | AssBoolComm  WhileVar  BExp
    | CompComm  Comm Comm
    | IfComm    BExp Comm Comm
    | WhileComm BExp Comm
    deriving Show

