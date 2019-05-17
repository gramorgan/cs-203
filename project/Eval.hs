
module Eval where
import Parser
import State
import qualified Data.Map as Map
import Control.Monad.State.Lazy

evalProgram :: [LispExpr] -> ([LispVal], LispState)
evalProgram prog = runState (mapM evalExpr prog) initialState

evalExpr :: LispExpr -> State LispState LispVal

evalExpr (FloatExpr n)  = return $ AtomVal $ FloatAtom  $ n
evalExpr (BoolExpr  b)  = return $ AtomVal $ BoolAtom   $ b
evalExpr (StrExpr   s)  = return $ AtomVal $ StrAtom    $ s

evalExpr (LambdaExpr params body) = return $ AtomVal $ LambdaAtom $ f
    where f = (\args -> do { state   <- get
                           ; put $ makeInnerState params state args
                           ; result  <- evalExpr body
                           ; put state
                           ; return result })

evalExpr (CallExpr funcExpr args) = do
    val <- evalExpr funcExpr
    argVals <- mapM evalExpr args
    case val of
        (AtomVal (LambdaAtom func)) -> func argVals
        _                           -> error "not callable"

evalExpr (IdentExpr ident) = do
    state <- get
    return $ case Map.lookup ident state of
        Nothing  -> error ("var '" ++ ident ++  "' not in scope")
        Just val -> val

evalExpr (DefineExpr ident expr) = do
    state <- get
    val   <- evalExpr expr
    put $ Map.insert ident val state
    return NoneVal


makeInnerState :: [LispIdent] -> LispState -> [LispVal] -> LispState
makeInnerState params outerState args
    | length params /= length args = error "arity mismatch"
    | otherwise = Map.union (Map.fromList $ zip params args) outerState

