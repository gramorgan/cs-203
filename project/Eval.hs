
module Eval where
import Parser
import qualified Data.Map as Map
import Control.Monad.State.Lazy

data LispVal
    = AtomVal LispAtom
    | ListVal [LispVal]
    | NoneVal

instance Show LispVal where
    show (AtomVal atom) = show atom
    show (ListVal list) = "(" ++ (unwords $ map show list) ++ ")"
    show NoneVal        = "<None>"

data LispAtom
    = LambdaAtom {getLambdaFunc :: [LispExpr] -> State LispState LispVal}
    | FloatAtom   Float

instance Show LispAtom where
    show (LambdaAtom f) = "<Function>"
    show (FloatAtom n)  = show n

type LispState = Map.Map LispIdent LispVal


evalExpr :: LispExpr -> State LispState LispVal

evalExpr (FloatExpr n)            = return $ AtomVal $ FloatAtom $ n

evalExpr (LambdaExpr params body) = return $ AtomVal $ LambdaAtom $ f
    where f = (\args -> do { state      <- get
                           ; evaledArgs <- mapM evalExpr args
                           ; put $ makeInnerState params state evaledArgs
                           ; result     <- evalExpr body
                           ; put state
                           ; return result })

evalExpr (CallExpr funcExpr args) = do
    val <- evalExpr funcExpr
    case val of
        (AtomVal (LambdaAtom func)) -> func args
        _                           -> error "not callable"

evalExpr (IdentExpr ident)        = do
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

