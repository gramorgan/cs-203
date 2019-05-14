
module Eval where
import Parser
import qualified Data.Map as Map
import Control.Monad.State

data LispVal
    = AtomVal LispAtom
    | ListVal [LispAtom]
    | NoneVal

instance Show LispVal where
    show (AtomVal atom) = show atom
    show (ListVal list) = "(" ++ (unwords $ map show list) ++ ")"

data LispAtom
    = LambdaAtom ([LispVal] -> LispVal)
    | FloatAtom   Float

instance Show LispAtom where
    show (LambdaAtom f) = "<Function>"
    show (FloatAtom n)  = show n

type LispState = Map.Map LispIdent LispVal

evalExpr :: LispState -> LispExpr -> LispVal

evalExpr state (FloatExpr n)              = AtomVal (FloatAtom  n)
evalExpr state (LambdaExpr params body)   = AtomVal (LambdaAtom (\ args -> evalExpr (makeFuncState params state args) body ))
evalExpr state (CallExpr funcExpr args) = case evalExpr state funcExpr of
    (AtomVal (LambdaAtom f)) -> f $ map (evalExpr state) args
    _                        -> error "not a function"
evalExpr state (IdentExpr ident)          = case Map.lookup ident state of
    Nothing  -> error ("var '" ++ ident ++  "' used before assignment")
    Just val -> val

makeFuncState :: [LispIdent] -> LispState -> [LispVal]  -> LispState
makeFuncState params outerState args
    | length params /= length args = error "arity mismatch"
    | otherwise = foldl (\ acc (arg, param) -> Map.insert param arg acc) outerState $ zip args params

-- could work
-- foldl (flip $) outerState $ zipWith Map.insert params args

