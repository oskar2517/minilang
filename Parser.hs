module Parser where

import Data.Maybe (fromMaybe)
import Lexer (Token (..))

-- Todo: clashes schöner fixen, implement bang
data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Lt
  | Gt
  | Eq
  | NotEq
  deriving (Show)

data ExpressionNode
  = BinaryExpressionNode Operator ExpressionNode ExpressionNode
  | UnaryExpressionNode Operator ExpressionNode
  | StringNode String
  | BooleanNode Bool
  | IdentNode String
  | NilNode
  | NumberNode Int
  deriving (Show)

data ReturnNode = ReturnNil | ReturnExpression ExpressionNode deriving (Show)

data StatementNode
  = BlockNode [StatementNode]
  | WhileNode ExpressionNode StatementNode
  | IfNode ExpressionNode StatementNode StatementNode
  | ExpressionStatementNode ExpressionNode
  | ReturnNode
  | VariableDeclarationNode String ExpressionNode
  | VariableAssignNode String ExpressionNode
  deriving (Show)

newtype AstNode = AstNode [StatementNode]

parseExpression :: [Token] -> Either (ExpressionNode, [Token]) String
parseExpression = parseComparison

parseComparison :: [Token] -> Either (ExpressionNode, [Token]) String
parseComparison tokens =
  case parseNumeric tokens of
    Right m -> Right m
    Left (left, rest) -> parseComparison' left rest
  where
    parseComparison' left tokens =
      case tokens of
        LessThan : rest ->
          case parseNumeric rest of
            Right m -> Right m
            Left (right, rest') -> parseComparison' (BinaryExpressionNode Lt left right) rest'
        GreaterThan : rest ->
          case parseNumeric rest of
            Right m -> Right m
            Left (right, rest') -> parseComparison' (BinaryExpressionNode Gt left right) rest'
        Equals : rest ->
          case parseNumeric rest of
            Right m -> Right m
            Left (right, rest') -> parseComparison' (BinaryExpressionNode Eq left right) rest'
        NotEquals : rest ->
          case parseNumeric rest of
            Right m -> Right m
            Left (right, rest') -> parseComparison' (BinaryExpressionNode NotEq left right) rest'
        _ -> Left (left, tokens)

parseNumeric :: [Token] -> Either (ExpressionNode, [Token]) String
parseNumeric tokens =
  case parseTerm tokens of
    Right m -> Right m
    Left (left, rest) -> parseNumeric' left rest
  where
    parseNumeric' left tokens =
      case tokens of
        Plus : rest ->
          case parseTerm rest of
            Right m -> Right m
            Left (right, rest') -> parseNumeric' (BinaryExpressionNode Add left right) rest'
        Minus : rest ->
          case parseTerm rest of
            Right m -> Right m
            Left (right, rest') -> parseNumeric' (BinaryExpressionNode Subtract left right) rest'
        _ -> Left (left, tokens)

parseTerm :: [Token] -> Either (ExpressionNode, [Token]) String
parseTerm tokens =
  case parseSignedFactor tokens of
    Right m -> Right m
    Left (left, rest) -> parseTerm' left rest
  where
    parseTerm' left tokens =
      case tokens of
        Asterisk : rest ->
          case parseSignedFactor rest of
            Right m -> Right m
            Left (right, rest') -> parseTerm' (BinaryExpressionNode Multiply left right) rest'
        Slash : rest ->
          case parseSignedFactor rest of
            Right m -> Right m
            Left (right, rest') -> parseTerm' (BinaryExpressionNode Divide left right) rest'
        Percent : rest ->
          case parseSignedFactor rest of
            Right m -> Right m
            Left (right, rest') -> parseTerm' (BinaryExpressionNode Modulo left right) rest'
        _ -> Left (left, tokens)

parseSignedFactor :: [Token] -> Either (ExpressionNode, [Token]) String
parseSignedFactor (Minus : t) = do
  case parseFactor t of
    Right m -> Right m
    Left (expr, rest) -> Left (UnaryExpressionNode Subtract expr, rest)
parseSignedFactor tokens = parseFactor tokens

parseFactor :: [Token] -> Either (ExpressionNode, [Token]) String
parseFactor (h : t) = case h of
  Number l -> Left (NumberNode (read l), t)
  Nil -> Left (NilNode, t)
  BoolTrue -> Left (BooleanNode True, t)
  BoolFalse -> Left (BooleanNode False, t)
  StringToken s -> Left (StringNode s, t)
  LParen -> case parseExpression t of
    Right m -> Right m
    Left (expr, h : t) -> if h == RParen then Left (expr, t) else Right "Unexpected token2"
  _ -> Right "Unexpected token"
