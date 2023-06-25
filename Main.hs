module Main where
import Parser (parseAst)

main :: IO ()
main = do
    code <- readFile "program.txt"
    let ast = parseAst code
    print ast