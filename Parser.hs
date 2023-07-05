module Parser (parseAst, StatementNode (..), ExpressionNode (..), Operator (..)) where

import Parsing

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Lt
  | Lte
  | Gt
  | Gte
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
  | NumberNode Float
  | ArrayNode [ExpressionNode]
  | ArrayAccessNode {target :: ExpressionNode, index :: ExpressionNode}
  | CallNode {target :: ExpressionNode, arguments :: [ExpressionNode]}
  | FunctionNode {parameters :: [String], body :: StatementNode}
  deriving (Show)

data StatementNode
  = BlockNode [StatementNode]
  | WhileNode ExpressionNode StatementNode
  | IfNode ExpressionNode StatementNode StatementNode
  | ExpressionStatementNode ExpressionNode
  | ReturnNode
  | VariableDeclarationNode String ExpressionNode
  | VariableAssignNode String ExpressionNode
  | PrintStatementNode ExpressionNode
  deriving (Show)

parseAst :: String -> Maybe StatementNode
parseAst code = do
  let ast = head $ parse (many statement) code
  if null $ snd ast
    then Just (BlockNode $ fst ast)
    else Nothing

statement :: Parser StatementNode
statement = do
  space
  s <- printStatement <|> variableDeclaration <|> variableAssign <|> if' <|> while <|> expressionStatement
  space
  return s

variableDeclaration :: Parser StatementNode
variableDeclaration = do
  token $ string "var"
  name <- ident
  token $ string "="
  value <- expression
  token $ char ';'
  return $ VariableDeclarationNode name value

printStatement :: Parser StatementNode
printStatement = do
  token $ string "print"
  expr <- expression
  token $ char ';'
  return $ PrintStatementNode expr

variableAssign :: Parser StatementNode
variableAssign = do
  name <- ident
  token $ char '='
  value <- expression
  token $ char ';'
  return $ VariableAssignNode name value

block :: Parser StatementNode
block = do
  token $ char '{'
  statements <- many statement
  token $ char '}'
  return $ BlockNode statements

if' :: Parser StatementNode
if' =
  do
    token $ string "if"
    condition <- expression
    consequence <- block
    token $ string "else"
    IfNode condition consequence <$> block
    <|> do
      token $ string "if"
      condition <- expression
      consequence <- block
      return $ IfNode condition consequence $ BlockNode []

expressionStatement :: Parser StatementNode
expressionStatement = do
  expr <- expression
  token $ char ';'
  return $ ExpressionStatementNode expr

while :: Parser StatementNode
while = do
  token $ string "while"
  condition <- expression
  WhileNode condition <$> block

expression :: Parser ExpressionNode
expression = comparison

comparison' :: ExpressionNode -> Parser ExpressionNode
comparison' left = do
  do
    token $ char '<'
    right <- term
    comparison' (BinaryExpressionNode Lt left right)
    <|> do
      token $ string "<="
      right <- term
      comparison' (BinaryExpressionNode Lte left right)
    <|> do
      token $ string ">"
      right <- term
      comparison' (BinaryExpressionNode Gt left right)
    <|> do
      token $ string ">="
      right <- term
      comparison' (BinaryExpressionNode Gte left right)
    <|> do
      token $ string "=="
      right <- term
      comparison' (BinaryExpressionNode Eq left right)
    <|> do
      token $ string "!="
      right <- term
      comparison' (BinaryExpressionNode NotEq left right)
    <|> do
      token $ char '>'
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
    token $ char '+'
    right <- term
    numeric' (BinaryExpressionNode Add left right)
    <|> do
      token $ char '-'
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
    token $ char '*'
    right <- signedFactor
    term' (BinaryExpressionNode Multiply left right)
    <|> do
      token $ char '/'
      right <- signedFactor
      term' (BinaryExpressionNode Divide left right)
    <|> do
      token $ char '%'
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
    token $ char '-'
    UnaryExpressionNode Subtract <$> access
    <|> access

access' :: ExpressionNode -> Parser ExpressionNode
access' left =
  do
    token $ char '['
    index <- expression
    token $ char ']'
    access' (ArrayAccessNode left index)
    <|> do
      token $ char '('
      arguments <- expressionList
      token $ char ')'
      access' (CallNode left arguments)
    <|> return left

access :: Parser ExpressionNode
access =
  do
    left <- factor
    access' left
    <|> factor

factor :: Parser ExpressionNode
factor = number <|> string' <|> boolean <|> function <|> identifier' <|> parenExpression <|> array

parenExpression :: Parser ExpressionNode
parenExpression = do
  token $ char '('
  expr <- expression
  token $ char ')'
  return expr

number :: Parser ExpressionNode
number = do
  n <-
    do
      i <- some digit
      token $ char '.'
      f <- some digit
      return $ i ++ "." ++ f
      <|> some digit
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

expressionList :: Parser [ExpressionNode]
expressionList =
  many $
    do
      expr <- expression
      token $ char ','
      return expr
      <|> expression

function :: Parser ExpressionNode
function = do
  token $ string "func"
  token $ char '('
  parameters <-
    many $
      do
        param <- ident
        token $ char ','
        return param
        <|> ident
  token $ char ')'
  FunctionNode parameters <$> block

array :: Parser ExpressionNode
array = do
  token $ char '['
  values <- expressionList
  token $ char ']'
  return $ ArrayNode values