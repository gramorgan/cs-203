
module Main where
import AST
import qualified Data.Map as Map
import Text.Printf

-- type for while expression states
type WhileState = Map.Map WhileVar Int

-- pretty print states
printState :: WhileState -> IO ()
printState state = mapM_ (\ (var, val) -> printf "%s = %d\n" var val) $ Map.toList state

-- evaluate arithmetic expressions
evalAExp :: AExp -> WhileState -> Int

evalAExp (IntAExp n)      state = n
evalAExp (VarAExp var)    state = case Map.lookup var state of
    Nothing  -> error ("var '" ++ var ++  "' used before assignment")
    Just val -> val
evalAExp (SumAExp  e1 e2) state = (evalAExp e1 state) + (evalAExp e2 state)
evalAExp (DiffAExp e1 e2) state = (evalAExp e1 state) - (evalAExp e2 state)
evalAExp (MulAExp  e1 e2) state = (evalAExp e1 state) * (evalAExp e2 state)

-- evalueate boolean expressions
evalBExp :: BExp -> WhileState -> Bool

evalBExp  TrueBExp       state = True
evalBExp  FalseBExp      state = False
evalBExp (EqBExp  e1 e2) state = (evalAExp e1 state) == (evalAExp e2 state)
evalBExp (LtBExp  e1 e2) state = (evalAExp e1 state) <  (evalAExp e2 state)
evalBExp (NotBExp e)     state =                    not (evalBExp e  state)
evalBExp (AndBExp e1 e2) state = (evalBExp e1 state) && (evalBExp e2 state)
evalBExp (OrBExp  e1 e2) state = (evalBExp e1 state) || (evalBExp e2 state)

-- evaluate commands
evalComm :: Comm -> WhileState -> WhileState

evalComm  SkipComm        state = state
evalComm (AssComm  var e) state = Map.insert var (evalAExp e state) state
evalComm (CompComm c1 c2) state = evalComm c2 $ evalComm c1 state
evalComm (IfComm e c1 c2) state = if evalBExp e state
                                      then (evalComm c1 state)
                                      else (evalComm c2 state)
evalComm (WhileComm e c)  state = if evalBExp e state
                                      then evalComm (WhileComm e c) (evalComm c state)
                                      else state

-- x := 2; i := 0; while i < 10 do (x := x*2; i := i+1)
main = printState $ evalComm (WhileComm (LtBExp (VarAExp "i") (IntAExp 10)) (CompComm (AssComm "x" (MulAExp (VarAExp "x") (IntAExp 2))) (AssComm "i" (SumAExp (VarAExp "i") (IntAExp 1))))) $ Map.fromList [("i", 0), ("x", 2)]

