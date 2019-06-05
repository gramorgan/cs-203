-- State.hs
-- types representing values of scheme expressions and states

module State where
import qualified Data.Map as Map
import Control.Monad.State.Lazy

-- values
data LispVal
    = NoneVal
    | QuoteVal   LispVal
    | ListVal   [LispVal]
    | LambdaVal  ([LispVal] -> State LispState LispVal)
    | FloatVal   Float
    | BoolVal    Bool
    | StrVal     String
    | IdentVal   String

instance Show LispVal where
    show NoneVal       = "<None>"
    show (QuoteVal e)  = "'" ++ (show e)
    show (LambdaVal f) = "<Function>"
    show (FloatVal  n) = show n
    show (BoolVal   b) = if b then "#t" else "#f"
    show (StrVal    s) = s
    show (ListVal   l) = "(" ++ (unwords $ map show l) ++ ")"

-- identifiers are just strings
type LispIdent = String

-- states are maps from idents to values
type LispState = Map.Map LispIdent LispVal

-- initial state - builtin functions
initialState = Map.fromList [ ("+"     , makeBinaryNumOp (+))
                            , ("-"     , makeBinaryNumOp (-))
                            , ("/"     , makeBinaryNumOp (/))
                            , ("*"     , makeBinaryNumOp (*))
                            , ("<"     , makeCmpOp (<))
                            , (">"     , makeCmpOp (>))
                            , ("<="    , makeCmpOp (<=))
                            , (">="    , makeCmpOp (>=))
                            , ("="     , makeCmpOp (==))
                            , ("or"    , makeBoolOp (||))
                            , ("and"   , makeBoolOp (&&))
                            , ("map"   , (LambdaVal lispMap))
                            , ("apply" , (LambdaVal lispApply))
                            , ("filter", (LambdaVal lispFilter))
                            ]

-- make a scheme arithmetic operator from a haskell function
makeBinaryNumOp :: (Float -> Float -> Float) -> LispVal
makeBinaryNumOp func = LambdaVal $ f
    where f = (\ args -> case args of
                [(FloatVal a), (FloatVal b)] -> return $ FloatVal $ func a b
                [_, _]                       -> error "type error"
                _                            -> error "arity mismatch")

-- make a scheme comparison operator from a haskell function
makeCmpOp :: (Float -> Float -> Bool) -> LispVal
makeCmpOp func = LambdaVal $ f
    where f = (\ args -> case args of
                [(FloatVal a), (FloatVal b)] -> return $ BoolVal $ func a b
                [_, _]                       -> error "type error"
                _                            -> error "arity mismatch")

-- make a boolean operator from a haskell function
makeBoolOp :: (Bool -> Bool -> Bool) -> LispVal
makeBoolOp func = LambdaVal $ f
    where f = (\ args -> case args of
                [(BoolVal a), (BoolVal b)] -> return $ BoolVal $ func a b
                [_, _]                       -> error "type error"
                _                            -> error "arity mismatch")

-- map
lispMap args = case args of
    [(LambdaVal f), (QuoteVal (ListVal l))] -> mapM (\ e -> f [e]) l >>= return . QuoteVal . ListVal
    [_, _]                                  -> error "type error"
    _                                       -> error "arity mismatch"

-- apply
lispApply args = case args of
    [(LambdaVal f), (QuoteVal (ListVal l))] -> f l
    [_, _]                                  -> error "type error"
    _                                       -> error "arity mismatch"

-- filter
lispFilter args = case args of 
    [(LambdaVal f), (QuoteVal (ListVal l))] -> filterM (filterFunc f) l >>= return . QuoteVal . ListVal
    [_, _]                                  -> error "type error"
    _                                       -> error "arity mismatch"
    where filterFunc f e = (f [e]) >>= (\ r -> case r of
                                                (BoolVal b) -> return b
                                                _           -> error "filter function must return a boolean")

