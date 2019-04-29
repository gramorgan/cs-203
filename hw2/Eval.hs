
-- evaluate While ASTs
-- uses the AST definition from AST.hs
module Eval where
import AST
import qualified Data.Map as Map
import Text.Printf

-- type representing the value of a while variable
data WhileVal
    = IntVal Int
    | BoolVal Bool
    deriving Eq

printVal (IntVal n)  = show n
printVal (BoolVal b) = show b

-- type for while expression states
type WhileState = Map.Map WhileVar WhileVal

-- pretty print states
printState :: WhileState -> IO ()
printState state = mapM_ (\ (var, val) -> printf "%s = %s\n" var $ printVal val) $ Map.toList state

-- evaluate arithmetic expressions
evalAExp :: WhileState -> AExp -> Int

evalAExp state (IntAExp n)      = n
evalAExp state (VarAExp var)    = case Map.lookup var state of
    Nothing            -> error ("var '" ++ var ++  "' used before assignment")
    Just (BoolVal _)   -> error ("type error: var " ++ var ++ " is of type bool, should be int")
    Just (IntVal  val) -> val
evalAExp state (SumAExp  e1 e2) = (evalAExp state e1) + (evalAExp state e2)
evalAExp state (DiffAExp e1 e2) = (evalAExp state e1) - (evalAExp state e2)
evalAExp state (MulAExp  e1 e2) = (evalAExp state e1) * (evalAExp state e2)

-- evalueate boolean expressions
evalBExp :: WhileState -> BExp -> Bool

evalBExp state (BoolBExp b)     = b
evalBExp state (VarBExp var)    = case Map.lookup var state of
    Nothing            -> error ("var '" ++ var ++  "' used before assignment")
    Just (IntVal  _)   -> error ("type error: var " ++ var ++ " is of type int, should be bool")
    Just (BoolVal val) -> val
evalBExp state (EqBExp  e1 e2) = (evalAExp state e1) == (evalAExp state e1)
evalBExp state (LtBExp  e1 e2) = (evalAExp state e1) <  (evalAExp state e2)
evalBExp state (NotBExp e)     =                    not (evalBExp state e )
evalBExp state (AndBExp e1 e2) = (evalBExp state e1) && (evalBExp state e2)
evalBExp state (OrBExp  e1 e2) = (evalBExp state e1) || (evalBExp state e2)

-- evaluate commands
evalComm :: WhileState -> Comm -> WhileState

evalComm state  SkipComm            = state
evalComm state (AssIntComm  var e)  = Map.insert var (IntVal (evalAExp state e)) state
evalComm state (AssBoolComm  var e) = Map.insert var (BoolVal (evalBExp state e)) state
evalComm state (CompComm c1 c2)     = evalComm (evalComm state c1) c2
evalComm state (IfComm e c1 c2)     = if evalBExp state e
                                          then (evalComm state c1)
                                          else (evalComm state c2)
evalComm state (WhileComm e c)      = if evalBExp state e
                                          then evalComm (evalComm state c) (WhileComm e c)
                                          else state

