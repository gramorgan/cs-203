
module Main where
import Parse
import Text.Printf

-- evaluate expressions
eval :: Exp -> Float
eval (FloatExp n)    = n
eval (SumExp e1 e2)  = (eval e1) + (eval e2)
eval (DiffExp e1 e2) = (eval e1) - (eval e2)
eval (MulExp e1 e2)  = (eval e1) * (eval e2)
eval (DivExp e1 e2)  = (eval e1) / (eval e2)
eval (InvExp e)      = 0 - (eval e)

-- interpret a string ARITH expression
interp :: String -> Float
interp = eval . parser . lexer

-- run a single test
runTest :: (String, Float) -> IO ()
runTest (expr, expectedResult)
    | result == expectedResult = printSuccess expr result
    | otherwise                = printFailure expr expectedResult result
    where result = interp expr

printSuccess :: String -> Float -> IO ()
printSuccess expr result = printf "%20s = %f\n" expr result

printFailure :: String -> Float -> Float -> IO ()
printFailure expr expectedResult result =
    printf "test failed: %s should equal %f but evaluated to %f\n" expr expectedResult result

-- run runTest on a bunch of different test cases
main = mapM_ runTest [ ("1+1", 2)
                     , ("(1+4)*6", 30)
                     , ("(1+4)/5", 1)
                     , ("((18*2+1)+1)/2", 19)
                     , ("1000/5", 200)
                     , ("500-35", 465)
                     , ("10000-(600*4)/8", 9700)
                     , ("11", 11)
                     , ("-11", -11)
                     , ("-(18*4+6)", -78)
                     , ("18+-8", 10)
                     , ("-900/9", -100)
                     , ("1200/40+3", 33)
                     , ("15+16+17", 48)
                     , ("64/2/2/2/2/2/2", 1)
                     , ("1/2", 0.5)
                     , ("8*3+101", 125)
                     , ("900/18", 50)
                     , ("-(6+3)+(-(1.5*6+2))", -20)
                     , ("0-1200", -1200)
                     ]

