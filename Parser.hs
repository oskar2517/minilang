module Parser (parseAst, StatementNode (..), ExpressionNode (..), Operator (..), expression) where

import Parsing (
    Alternative (many, some, (<|>)),
    Parser,
    char,
    digit,
    ident,
    letter,
    parse,
    space,
    string,
    token,
 )

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
    | ArrayAccessNode ExpressionNode ExpressionNode
    deriving (Show)

data StatementNode
    = BlockNode [StatementNode]
    | WhileNode ExpressionNode StatementNode
    | IfNode ExpressionNode StatementNode StatementNode
    | ReturnNode
    | VariableDeclarationNode String ExpressionNode
    | VariableAssignNode String ExpressionNode
    | PrintStatementNode ExpressionNode
    | ArrayAssignNode ExpressionNode ExpressionNode
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
    s <- printStatement <|> variableDeclaration <|> variableAssign <|> arrayAssign <|> if' <|> while
    space
    return s

printStatement :: Parser StatementNode
printStatement = do
    token $ string "print"
    expr <- expression
    token $ char ';'
    return $ PrintStatementNode expr

variableDeclaration :: Parser StatementNode
variableDeclaration = do
    token $ string "var"
    name <- ident
    token $ string "="
    value <- expression
    token $ char ';'
    return $ VariableDeclarationNode name value

variableAssign :: Parser StatementNode
variableAssign = do
    name <- ident
    token $ char '='
    value <- expression
    token $ char ';'
    return $ VariableAssignNode name value

arrayAssign :: Parser StatementNode
arrayAssign = do
    target <- access
    token $ char '='
    value <- expression
    token $ char ';'
    return $ ArrayAssignNode target value

if' :: Parser StatementNode
if' = ifWithElse <|> ifWithoutElse

ifWithElse :: Parser StatementNode
ifWithElse = do
    token $ string "if"
    condition <- expression
    consequence <- block
    token $ string "else"
    IfNode condition consequence <$> block

ifWithoutElse :: Parser StatementNode
ifWithoutElse = do
    token $ string "if"
    condition <- expression
    consequence <- block
    return $ IfNode condition consequence $ BlockNode []

while :: Parser StatementNode
while = do
    token $ string "while"
    condition <- expression
    WhileNode condition <$> block

block :: Parser StatementNode
block = do
    token $ char '{'
    statements <- many statement
    token $ char '}'
    return $ BlockNode statements

expression :: Parser ExpressionNode
expression = comparison

comparison :: Parser ExpressionNode
comparison = do
    left <- numeric
    comparison' left

comparison' :: ExpressionNode -> Parser ExpressionNode
comparison' left =
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

numeric :: Parser ExpressionNode
numeric = do
    left <- term
    numeric' left

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

term :: Parser ExpressionNode
term = do
    left <- signedFactor
    term' left

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

signedFactor :: Parser ExpressionNode
signedFactor =
    do
        token $ char '-'
        UnaryExpressionNode Subtract <$> access
        <|> access

access :: Parser ExpressionNode
access =
    do
        left <- factor
        access' left
        <|> factor

access' :: ExpressionNode -> Parser ExpressionNode
access' left =
    do
        token $ char '['
        index <- expression
        token $ char ']'
        access' (ArrayAccessNode left index)
        <|> return left

factor :: Parser ExpressionNode
factor = number <|> string' <|> boolean <|> nil <|> identifier' <|> parenExpression <|> array

number :: Parser ExpressionNode
number = float <|> integer'

integer' :: Parser ExpressionNode
integer' = do
    n <- some digit
    return $ NumberNode $ read n

float :: Parser ExpressionNode
float = do
    i <- some digit
    token $ char '.'
    f <- some digit
    return $ NumberNode $ read $ i ++ "." ++ f

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

parenExpression :: Parser ExpressionNode
parenExpression = do
    token $ char '('
    expr <- expression
    token $ char ')'
    return expr

array :: Parser ExpressionNode
array = do
    token $ char '['
    values <- expressionList
    token $ char ']'
    return $ ArrayNode values

expressionList :: Parser [ExpressionNode]
expressionList =
    many $
        do
            expr <- expression
            token $ char ','
            return expr
            <|> expression