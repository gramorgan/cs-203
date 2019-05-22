module Main where
import Lexer
import Parser
import State
import Eval
import System.IO
import Control.Monad(unless)
import Control.Exception

readAndEval state = do
    inString <- getLine
    let (result, newState) = (evalProgram state . parser . alexScanTokens) $ inString
    print $ last result
    printPrompt newState

printPrompt state = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    unless eof $ readAndEval state

main = printPrompt initialState
