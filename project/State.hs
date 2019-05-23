
module State where
import Parser(LispExpr)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

data LispVal
    = NoneVal
    | QuoteVal   LispExpr
    | LambdaVal  ([LispVal] -> State LispState LispVal)
    | FloatVal   Float
    | BoolVal    Bool
    | StrVal     String

instance Show LispVal where
    show NoneVal       = "<None>"
    show (QuoteVal e)  = "'" ++ (show e)
    show (LambdaVal f) = "<Function>"
    show (FloatVal  n) = show n
    show (BoolVal   b) = if b then "#t" else "#f"
    show (StrVal    s) = s

type LispIdent = String

type LispState = Map.Map LispIdent LispVal

initialState = Map.fromList [ ("+"  , makeBinaryNumOp (+))
                            , ("-"  , makeBinaryNumOp (-))
                            , ("/"  , makeBinaryNumOp (/))
                            , ("*"  , makeBinaryNumOp (*))
                            , ("<"  , makeCmpOp (<))
                            , (">"  , makeCmpOp (>))
                            , ("<=" , makeCmpOp (<=))
                            , (">=" , makeCmpOp (>=))
                            , ("="  , makeCmpOp (==))
                            ]

makeBinaryNumOp :: (Float -> Float -> Float) -> LispVal
makeBinaryNumOp func = LambdaVal $ f
    where f = (\ args -> case args of
                [(FloatVal a), (FloatVal b)] -> return $ FloatVal $ func a b
                [_, _]                       -> error "type error"
                _                            -> error "arity mismatch")

makeCmpOp :: (Float -> Float -> Bool) -> LispVal
makeCmpOp func = LambdaVal $ f
    where f = (\ args -> case args of
                [(FloatVal a), (FloatVal b)] -> return $ BoolVal $ func a b
                [_, _]                       -> error "type error"
                _                            -> error "arity mismatch")

