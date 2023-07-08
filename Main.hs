module Main where

import Control.Monad (void)
import Interpreter (execute)
import Parser (expression, parseAst)

import Parsing

main :: IO ()
main = do
    code <- readFile "program.txt"
    let ast = parseAst code
    case ast of
        Nothing -> error "Syntax error in file"
        Just a -> do
            void $ execute a
