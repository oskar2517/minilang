module Parser (parseAst) where
    
import Parsing

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

data StatementNode
  = BlockNode [StatementNode]
  | WhileNode ExpressionNode StatementNode
  | IfNode ExpressionNode StatementNode StatementNode
  | ExpressionStatementNode ExpressionNode
  | ReturnNode
  | VariableDeclarationNode String ExpressionNode
  | VariableAssignNode String ExpressionNode
  deriving (Show)

parseAst :: String -> [([StatementNode], String)]
parseAst = parse $ many statement

keyword :: String -> Parser ()
keyword k = do
  space
  string k
  space

statement :: Parser StatementNode
statement = do
  space
  s <- variableDeclaration <|> variableAssign <|> if' <|> while
  space
  return s

variableDeclaration :: Parser StatementNode
variableDeclaration = do
  keyword "var"
  name <- ident
  keyword "="
  value <- expression
  keyword ";"
  return $ VariableDeclarationNode name value

variableAssign :: Parser StatementNode
variableAssign = do
  name <- ident
  keyword "="
  value <- expression
  keyword ";"
  return $ VariableAssignNode name value

block :: Parser StatementNode
block = do
  keyword "{"
  statements <- many statement
  keyword "}"
  return $ BlockNode statements

if' :: Parser StatementNode
if' =
  do
    keyword "if"
    condition <- expression
    consequence <- block
    keyword "else"
    IfNode condition consequence <$> block
    <|> do
      keyword "if"
      condition <- expression
      consequence <- block
      return $ IfNode condition consequence $ BlockNode []

while :: Parser StatementNode
while = do
    keyword "while"
    condition <- expression
    WhileNode condition <$> block

expression :: Parser ExpressionNode
expression = comparison

comparison' :: ExpressionNode -> Parser ExpressionNode
comparison' left = do
  do
    keyword "<"
    right <- term
    comparison' (BinaryExpressionNode Lt left right)
    <|> do
      keyword ">"
      right <- term
      comparison' (BinaryExpressionNode Gt left right)
    <|> do
      keyword "=="
      right <- term
      comparison' (BinaryExpressionNode Eq left right)
    <|> do
      keyword ">"
      right <- term
      comparison' (BinaryExpressionNode NotEq left right)
    <|> return left

comparison :: Parser ExpressionNode
comparison = do
  left <- numeric
  comparison' left

numeric' :: ExpressionNode -> Parser ExpressionNode
numeric' left =
  do
    keyword "+"
    right <- term
    numeric' (BinaryExpressionNode Add left right)
    <|> do
      keyword "-"
      right <- term
      numeric' (BinaryExpressionNode Subtract left right)
    <|> return left

numeric :: Parser ExpressionNode
numeric = do
  left <- term
  numeric' left

term' :: ExpressionNode -> Parser ExpressionNode
term' left =
  do
    keyword "*"
    right <- signedFactor
    term' (BinaryExpressionNode Multiply left right)
    <|> do
      keyword "/"
      right <- signedFactor
      term' (BinaryExpressionNode Divide left right)
    <|> do
      keyword "%"
      right <- signedFactor
      term' (BinaryExpressionNode Modulo left right)
    <|> return left

term :: Parser ExpressionNode
term = do
  left <- signedFactor
  term' left

signedFactor :: Parser ExpressionNode
signedFactor =
  do
    keyword "-"
    UnaryExpressionNode Subtract <$> factor
    <|> factor

factor :: Parser ExpressionNode
factor = number <|> string' <|> boolean <|> identifier' <|> do
    keyword "("
    expr <- expression
    keyword ")"
    return expr

number :: Parser ExpressionNode
number = do
  n <- some digit
  return $ NumberNode $ read n

string' :: Parser ExpressionNode
string' = do
  char '"'
  s <- some letter
  char '"'
  return $ StringNode s

boolean :: Parser ExpressionNode
boolean = do
  b <- string "true" <|> string "false"
  return $ BooleanNode $ b == "true"

nil :: Parser ExpressionNode
nil = do
  string "nil"
  return NilNode

identifier' :: Parser ExpressionNode
identifier' = IdentNode <$> ident