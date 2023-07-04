module Main where
import Parser (parseAst)
import Interpreter (execute, Context)
import Control.Monad (void)

main :: IO ()
main = do
    code <- readFile "program.txt"
    let ast = parseAst code
    case ast of
        Nothing -> error "Syntax error in file"
        Just a -> do 
            void $ execute a