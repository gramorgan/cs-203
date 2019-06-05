
-- repl - entry point for interpreter

module Main where
import Lexer
import Parser
import State
import Eval
import System.IO
import Control.Monad(unless)
import Control.Exception

-- read a single line of input, evaluate it and print the prompt again
readAndEval state = do
    inString <- getLine
    let (result, newState) = (evalProgram state . parser . alexScanTokens) $ inString
    print $ last result
    printPrompt newState

-- print the prompt and wait for some input
printPrompt state = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    unless eof $ readAndEval state

main = printPrompt initialState
