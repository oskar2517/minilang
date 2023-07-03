module Main where
import Parser (parseAst)
import Interpreter (execute, Context)

main :: IO Context
main = do
    code <- readFile "program.txt"
    let ast = parseAst code
    case ast of
        Nothing -> error "Syntax error in file"
        Just a -> execute a