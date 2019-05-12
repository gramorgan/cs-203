
module Main where

import Eval
import AST
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Char (toLower)

-- convert commands to strings
showComm :: Comm -> String
showComm  SkipComm        = "skip"
showComm (AssComm var a)  = var ++ " := " ++ (showAExp a)
showComm (CompComm c1 c2) = (showComm c1) ++ " ; " ++ (showComm c2)
showComm (IfComm b c1 c2) = "if (" ++ (showBExp b ) ++
                            ") then (" ++ (showComm c1) ++
                            ") else (" ++ (showComm c2) ++ ")"
showComm (WhileComm b c) =  "while (" ++ (showBExp b) ++
                            ") do (" ++ (showComm c) ++ ")"


-- convert arithmetic expressions to strings
showAExp :: AExp -> String
showAExp (IntAExp  n)     = show n
showAExp (VarAExp  var)   = var
showAExp (SumAExp  e1 e2) = (showAExp e1) ++ "+" ++ (showAExp e2)
showAExp (DiffAExp e1 e2) = (showAExp e1) ++ "-" ++ (showAExp e2)
showAExp (MulAExp  e1 e2) = (showAExp e1) ++ "*" ++ (showAExp e2)

-- convert boolean expressions to strings
showBExp :: BExp -> String
showBExp (BoolBExp b)    = map toLower . show $ b
showBExp (EqBExp  e1 e2) = (showAExp e1) ++ "=" ++ (showAExp e2)
showBExp (LtBExp  e1 e2) = (showAExp e1) ++ "<" ++ (showAExp e2)
showBExp (NotBExp e)     = "!" ++ (showBExp e)
showBExp (AndBExp e1 e2) = (showBExp e1) ++ "&" ++ (showBExp e2)
showBExp (OrBExp  e1 e2) = (showBExp e1) ++ "|" ++ (showBExp e2)

-- pretty print states
showState :: WhileState -> String
showState state = "{" ++
                  (intercalate ", " $ map (\ (var, val) -> var ++ ": " ++ show val) $ Map.toList state) ++
                  "}"

-- evaluate a command, printing all intermediate commands
evalComm :: (Comm, WhileState) -> IO ()
evalComm (c, state) = do
    putStr "<"
    putStr . showComm $ c
    putStr ", "
    putStr . showState $ state
    putStrLn ">"
    if c == SkipComm && c' == SkipComm
        then putStr "\n"
        else evalComm (c', state')
    where (c', state') = stepComm $ (c, state)

main = mapM_ (\ c -> evalComm (c, Map.empty)) [
    (CompComm (AssComm "x" (IntAExp 3)) (IfComm (LtBExp (VarAExp "x") (IntAExp 5)) (AssComm "x" (SumAExp (VarAExp "x") (IntAExp 1))) (AssComm "x" (DiffAExp (VarAExp "x") (IntAExp 1))))) ,
    (CompComm (AssComm "myVar" (IntAExp 0)) (WhileComm (LtBExp (VarAExp "myVar") (IntAExp 3)) (AssComm "myVar" (SumAExp (VarAExp "myVar") (IntAExp 1))))) ,
    (CompComm (AssComm "z" (IntAExp 100)) (IfComm (NotBExp (LtBExp (IntAExp 30) (VarAExp "z"))) (AssComm "y" (IntAExp 30)) (AssComm "y" (VarAExp "z")))) ,
    (CompComm (AssComm "theNumber" (IntAExp 3)) (CompComm (AssComm "theOtherNumber" (IntAExp 4)) (AssComm "theSum" (SumAExp (VarAExp "theNumber") (VarAExp "theOtherNumber")))))
    ]

