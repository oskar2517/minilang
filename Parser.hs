module Parser where

import Data.Maybe (fromMaybe)
import Lexer (Token (..))

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
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
parseExpression = parseNumeric

parseNumeric :: [Token] -> Either (ExpressionNode, [Token]) String
parseNumeric tokens =
  case parseFactor tokens of
    Right m -> Right m
    Left (left, rest) -> parseNumeric' left rest
  where
    parseNumeric' left tokens =
      case tokens of
        Plus : rest ->
          case parseFactor rest of
            Right m -> Right m
            Left (right, rest') -> parseNumeric' (BinaryExpressionNode Add left right) rest'
        Minus : rest ->
          case parseFactor rest of
            Right m -> Right m
            Left (right, rest') -> parseNumeric' (BinaryExpressionNode Subtract left right) rest'
        _ -> Left (left, tokens)

parseFactor :: [Token] -> Either (ExpressionNode, [Token]) String
parseFactor (h : t) = case h of
  Number l -> Left (NumberNode (read l), t)
  Nil -> Left (NilNode, t)
  BoolTrue -> Left (BooleanNode True, t)
  BoolFalse -> Left (BooleanNode False, t)
  StringToken s -> Left (StringNode s, t)
  LParen -> case parseExpression t of
    Right m -> Right m
    Left (expr, h : t) -> if h == RParen then Left (expr, t) else Right "Unexpected token"
  _ -> Right "Unexpected token"
