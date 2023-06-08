module Lexer (Token (..), lexer) where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)

data Token
  = Number String
  | Ident String
  | StringToken String
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Percent
  | Assign
  | Equals
  | If
  | Else
  | While
  | Var
  | Nil
  | BoolTrue
  | BoolFalse
  | Illegal
  | Comma
  | LessThan
  | GreaterThan
  | Eof
  deriving (Show, Eq)

-- TODO: error recovery für tail. zb wenn string nicht geschlossen
lexer :: String -> [Token]
lexer [] = [Eof]
lexer all@(h : t)
  | h == ';' = Semicolon : lexer t
  | h == '(' = LParen : lexer t
  | h == ')' = RParen : lexer t
  | h == '{' = LBrace : lexer t
  | h == '}' = RBrace : lexer t
  | h == '+' = Plus : lexer t
  | h == '-' = Minus : lexer t
  | h == '*' = Asterisk : lexer t
  | h == '/' = Slash : lexer t
  | h == '%' = Percent : lexer t
  | h == ',' = Comma : lexer t
  | h == '<' = LessThan : lexer t
  | h == '>' = GreaterThan : lexer t
  | h == '=' = if h == '=' && head t == '=' then Equals : lexer (tail t) else Assign : lexer t
  | h == '"' = let (chars, t') = span (/= '"') t in StringToken chars : lexer (tail t')
  | isSpace h = lexer t
  | isDigit h = let (digits, t') = span isDigit t in Number (h : digits) : lexer t'
  | isLetter h = do
      let (ident, t') = span isAlphaNum all
       in case ident of
            "if" -> If : lexer t'
            "else" -> Else : lexer t'
            "while" -> While : lexer t'
            "var" -> Var : lexer t'
            "nil" -> Nil : lexer t'
            "true" -> BoolTrue : lexer t'
            "false" -> BoolFalse : lexer t'
            _ -> Ident ident : lexer t'
  | otherwise = Illegal : lexer t
