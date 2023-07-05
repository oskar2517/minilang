module Main where
import Parser (parseAst, expression)
import Interpreter (execute)
import Control.Monad (void)

import Parsing

main :: IO ()
main = do
    code <- readFile "program.txt"
    let ast = parseAst code
    case ast of
        Nothing -> error "Syntax error in file"
        Just a -> do 
            void $ execute a