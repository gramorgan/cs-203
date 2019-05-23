
module Eval where
import Parser
import State
import qualified Data.Map as Map
import Control.Monad.State.Lazy

evalProgram :: LispState -> [LispExpr] -> ([LispVal], LispState)
evalProgram state prog = runState (mapM evalExpr prog) state

evalExpr :: LispExpr -> State LispState LispVal

evalExpr (FloatExpr n)  = return $ FloatVal $ n
evalExpr (BoolExpr  b)  = return $ BoolVal  $ b
evalExpr (StrExpr   s)  = return $ StrVal   $ s

evalExpr (LambdaExpr params body) = return $ LambdaVal $ f
    where f = (\args -> do { state   <- get
                           ; put $ makeInnerState params state args
                           ; result  <- evalExpr body
                           ; put state
                           ; return result })

evalExpr (CallExpr funcExpr args) = do
    val <- evalExpr funcExpr
    argVals <- mapM evalExpr args
    case val of
        (LambdaVal func) -> func argVals
        _                 -> error "not callable"

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

evalExpr (QuoteExpr expr) = return (QuoteVal expr)

evalExpr (IfExpr be e1 e2) = do
    cond <- evalExpr be
    case cond of
        (BoolVal b) -> if b then evalExpr e1 else evalExpr e2
        _            -> error "condition must be a boolean"

evalExpr (LetExpr letList body) = do
    let idents = map fst letList
    vals <- mapM evalExpr $ map snd letList
    state <- get
    put $ makeInnerState idents state vals
    result <- evalExpr body
    put state
    return result


makeInnerState :: [LispIdent] -> LispState -> [LispVal] -> LispState
makeInnerState params outerState args
    | length params /= length args = error "arity mismatch"
    | otherwise = Map.union (Map.fromList $ zip params args) outerState

