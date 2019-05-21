
module State where
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
    = LambdaAtom ([LispVal] -> State LispState LispVal)
    | FloatAtom   Float
    | BoolAtom    Bool
    | StrAtom     String

instance Show LispAtom where
    show (LambdaAtom f) = "<Function>"
    show (FloatAtom  n) = show n
    show (BoolAtom   b) = if b then "#t" else "#f"
    show (StrAtom    s) = show s

type LispIdent = String

type LispState = Map.Map LispIdent LispVal

initialState = Map.fromList $ map (\ (k, v) -> (k, makeBinaryNumOp v) ) [("+"  , (+))
                                                                        ,("-"  , (-))
                                                                        ,("/"  , (/))
                                                                        ,("*"  , (/))
                                                                        ]

makeBinaryNumOp :: (Float -> Float -> Float) -> LispVal
makeBinaryNumOp func = AtomVal $ LambdaAtom $ f
    where f = (\ args -> case args of
                [(AtomVal (FloatAtom a)), (AtomVal (FloatAtom b))] ->
                    return $ AtomVal $ FloatAtom $ func a b
                [_, _] -> error "type error"
                _      -> error "arity mismatch")

