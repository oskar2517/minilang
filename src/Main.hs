module Main where

import Control.Monad (void)
import Error (genericError)
import Interpreter (execute)
import Parser (expression, parseAst)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            die "Syntax: interpreter <file>"
        else do
            code <- readFile $ head args
            let ast = parseAst code
            case ast of
                Nothing -> genericError "Syntax error in file"
                Just a -> void $ execute a
