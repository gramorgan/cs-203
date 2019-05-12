
-- evaluate While ASTs
-- uses the AST definition from AST.hs
module Eval where
import AST
import qualified Data.Map as Map
import Text.Printf

-- type for while expression states
type WhileState = Map.Map WhileVar Int

-- evaluate arithmetic expressions
evalAExp :: WhileState -> AExp -> Int

evalAExp state (IntAExp n)      = n
evalAExp state (VarAExp var)    = case Map.lookup var state of
    Nothing  -> error ("var '" ++ var ++  "' used before assignment")
    Just val -> val
evalAExp state (SumAExp  e1 e2) = (evalAExp state e1) + (evalAExp state e2)
evalAExp state (DiffAExp e1 e2) = (evalAExp state e1) - (evalAExp state e2)
evalAExp state (MulAExp  e1 e2) = (evalAExp state e1) * (evalAExp state e2)

-- evalueate boolean expressions
evalBExp :: WhileState -> BExp -> Bool

evalBExp state (BoolBExp b)    = b
evalBExp state (EqBExp  e1 e2) = (evalAExp state e1) == (evalAExp state e1)
evalBExp state (LtBExp  e1 e2) = (evalAExp state e1) <  (evalAExp state e2)
evalBExp state (NotBExp e)     =                    not (evalBExp state e )
evalBExp state (AndBExp e1 e2) = (evalBExp state e1) && (evalBExp state e2)
evalBExp state (OrBExp  e1 e2) = (evalBExp state e1) || (evalBExp state e2)

-- evaluate commands
stepComm :: (Comm, WhileState) -> (Comm, WhileState)

stepComm ( SkipComm       , state) = (SkipComm, state)
stepComm ((AssComm  var a), state) = (SkipComm, Map.insert var (evalAExp state a ) state)
stepComm ((CompComm c1 c2), state)
    | c1 == SkipComm               = ( c2              , state')
    | otherwise                    = ((CompComm c1' c2), state')
    where (c1', state') = stepComm (c1, state)
stepComm ((IfComm b c1 c2), state) = if evalBExp state b then (c1, state) else (c2, state)
stepComm ((WhileComm b c) , state) = ((IfComm b (CompComm c (WhileComm b c)) SkipComm), state)

