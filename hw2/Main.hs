
module Main where

import Eval
import Tokens
import Parse
import AST
import qualified Data.Map as Map
import Text.Printf

interp :: String -> WhileState
interp = (evalComm Map.empty) . parser . lexer

-- run a single test
runTest :: (String, WhileVar, WhileVal) -> IO ()
runTest (expr, var, expectedVal) = if (testStateValue var expectedVal $ interp expr) 
                                      then printSuccess expr var expectedVal
                                      else printFailure expr var expectedVal

printSuccess expr var expectedVal = printf "test passed: \"%s\" - var \"%s\" equals %s\n" expr var $ printVal expectedVal
printFailure expr var expectedVal = printf "test failed: \"%s\" - var \"%s\" should equal %s\n" expr var $ printVal expectedVal

testStateValue :: WhileVar -> WhileVal -> WhileState -> Bool
testStateValue var expectedVal state = case Map.lookup var state of
    Nothing  -> False
    Just val -> val == expectedVal

-- run runTest on a bunch of different test cases
main = mapM_ runTest [ ("int x := 1", "x", (IntVal 1))
                     , ("int a := 1; int  b := 1; int n := 0; while n<10 do (int t := b; int b := a + b; int a := t; int n := n+1)"
                       , "b", (IntVal 144))
                     , ("int var := 100; if var < 150 then int var := var - 100 else skip", "var", (IntVal 0))
                     , ("int x := 15; bool myVar := ~(x < 15)", "myVar", (BoolVal True))
                     , ("int num := 2; bool flag := true; while flag do (int num := num * 2; if ~ num < 100 then bool flag := false else skip)"
                       , "num", (IntVal 128))
                     , ("int num := 1000; while ~ num < 1 do int num := num - 100", "num", (IntVal 0))
                     ]

