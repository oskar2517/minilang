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
    token, alphanum, sat,
 )
import Data.Char (isAscii)

-- | Represents various operators used in expressions.
data Operator
    = Add         -- ^ Addition operator (+)
    | Subtract    -- ^ Subtraction operator (-)
    | Multiply    -- ^ Multiplication operator (*)
    | Divide      -- ^ Division operator (/)
    | Modulo      -- ^ Modulo operator (%)
    | Lt          -- ^ Less than operator (<)
    | Lte         -- ^ Less than or equal to operator (<=)
    | Gt          -- ^ Greater than operator (>)
    | Gte         -- ^ Greater than or equal to operator (>=)
    | Eq          -- ^ Equality operator (==)
    | NotEq       -- ^ Inequality operator (/=)
    deriving (Show)

-- | Represents various nodes in an expression.
data ExpressionNode
    = BinaryExpressionNode Operator ExpressionNode ExpressionNode  -- ^ Binary expression node, consisting of an operator and two sub-expressions.
    | UnaryExpressionNode Operator ExpressionNode                  -- ^ Unary expression node, consisting of an operator and a sub-expression.
    | StringNode String                                            -- ^ String literal node.
    | BooleanNode Bool                                             -- ^ Boolean literal node.
    | IdentNode String                                             -- ^ Identifier node.
    | NilNode                                                      -- ^ Nil literal node.
    | NumberNode Float                                             -- ^ Numeric literal node.
    | ArrayNode [ExpressionNode]                                   -- ^ Array literal node, consisting of a list of expressions.
    | ArrayAccessNode ExpressionNode ExpressionNode                -- ^ Array access node, representing accessing an element in an array using an index expression.
    deriving (Show)

-- | Represents various nodes in a statement.
data StatementNode
    = BlockNode [StatementNode]                                    -- ^ Block node, consisting of a list of statements.
    | WhileNode ExpressionNode StatementNode                       -- ^ While loop node, consisting of a loop condition expression and a loop body statement.
    | IfNode ExpressionNode StatementNode StatementNode            -- ^ If statement node, consisting of a condition expression, a consequence statement, and an optional alternative statement.
    | VariableDeclarationNode String ExpressionNode                -- ^ Variable declaration node, representing the declaration of a variable with an initial value expression.
    | VariableAssignNode String ExpressionNode                     -- ^ Variable assignment node, representing the assignment of a value expression to a variable.
    | PrintStatementNode ExpressionNode                            -- ^ Print statement node, representing the output of an expression.
    | ArrayAssignNode ExpressionNode ExpressionNode                -- ^ Array assignment node, representing the assignment of a value expression to an element in an array.
    deriving (Show)

-- | Parses the input code and returns a 'Just' value containing the parsed AST if successful, or 'Nothing' otherwise.
--
--   This function parses a series of statements and returns the top-level block node.
--
--   Example:
--
--   >>> parseAst "var x = 5; print x;"
--   Just (BlockNode [VariableDeclarationNode "x" (NumberNode 5.0),PrintStatementNode (IdentNode "x")])
parseAst :: String             -- ^ The code to parse
         -> Maybe StatementNode -- ^ The parsed AST (or 'Nothing' if parsing fails)
parseAst code = do
    let ast = head $ parse (many statement) code
    if null $ snd ast
        then Just (BlockNode $ fst ast)
        else Nothing

-- | Parses a statement.
--
--   This function consumes leading whitespace, parses a single statement, and then consumes trailing whitespace.
--
--   Example:
--
--   >>> parse statement "print x;"
--   [(PrintStatementNode (IdentNode "x"),"")]
statement :: Parser StatementNode  -- ^ The parsed statement
statement = do
    space
    s <- printStatement <|> variableDeclaration <|> variableAssign <|> arrayAssign <|> if' <|> while
    space
    return s

-- | Parses a print statement.
--
--   Example:
--
--   >>> parse printStatement "print x;"
--   [(PrintStatementNode (IdentNode "x"),"")]
printStatement :: Parser StatementNode  -- ^ The parsed print statement
printStatement = do
    token $ string "print"
    expr <- expression
    token $ char ';'
    return $ PrintStatementNode expr

-- | Parses a variable declaration statement.
--
--   Example:
--
--   >>> parse variableDeclaration "var x = 5;"
--   [(VariableDeclarationNode "x" (NumberNode 5.0),"")]
variableDeclaration :: Parser StatementNode  -- ^ The parsed variable declaration statement
variableDeclaration = do
    token $ string "var"
    name <- ident
    token $ string "="
    value <- expression
    token $ char ';'
    return $ VariableDeclarationNode name value

-- | Parses a variable assignment statement.
--
--   Example:
--
--   >>> parse variableAssign "x = 10;"
--   [(VariableAssignNode "x" (NumberNode 10.0),"")]
variableAssign :: Parser StatementNode  -- ^ The parsed variable assignment statement
variableAssign = do
    name <- ident
    token $ char '='
    value <- expression
    token $ char ';'
    return $ VariableAssignNode name value

-- | Parses an array assignment statement.
--
--   Example:
--
--   >>> parse arrayAssign "array[2] = x;"
--   [(ArrayAssignNode (ArrayAccessNode (IdentNode "array") (NumberNode 2.0)) (IdentNode "x"),"")]
arrayAssign :: Parser StatementNode  -- ^ The parsed array assignment statement
arrayAssign = do
    target <- access
    token $ char '='
    value <- expression
    token $ char ';'
    return $ ArrayAssignNode target value

-- | Parses an if statement with an optional else clause.
--
--   The function combines the parsers for if statements with and without else clauses.
--
--   Example:
--
--   >>> parse if' "if x > 0 { print \"Positive\"; } else { print \"Negative\"; }"
--   [(IfNode (BinaryExpressionNode Gt (IdentNode "x") (NumberNode 0.0)) (BlockNode [PrintStatementNode (StringNode "Positive")]) (BlockNode [PrintStatementNode (StringNode "Negative")]),"")]
if' :: Parser StatementNode  -- ^ The parsed if statement
if' = ifWithElse <|> ifWithoutElse

-- | Parses an if statement with an else clause.
--
--
--   Example:
--
--   >>> parse ifWithElse "if x > 0 { print \"Positive\"; } else { print \"Negative\"; }"
--   [(IfNode (BinaryExpressionNode Gt (IdentNode "x") (NumberNode 0.0)) (BlockNode [PrintStatementNode (StringNode "Positive")]) (BlockNode [PrintStatementNode (StringNode "Negative")]),"")]
ifWithElse :: Parser StatementNode  -- ^ The parsed if statement
ifWithElse = do
    token $ string "if"
    condition <- expression
    consequence <- block
    token $ string "else"
    IfNode condition consequence <$> block

-- | Parses an if statement without an else clause. An empty block is set as the alternative.
--
--
--   Example:
--
--   >>> parse ifWithoutElse "if x > 0 { print \"Positive\"; }"
--   [(IfNode (BinaryExpressionNode Gt (IdentNode "x") (NumberNode 0.0)) (BlockNode [PrintStatementNode (StringNode "Positive")]) (BlockNode []),"")]
ifWithoutElse :: Parser StatementNode  -- ^ The parsed if statement
ifWithoutElse = do
    token $ string "if"
    condition <- expression
    consequence <- block
    return $ IfNode condition consequence $ BlockNode []

-- | Parses a while loop statement.
--
--
--   Example:
--
--   >>> parse while "while x > 0 { print x; }"
--   [(WhileNode (BinaryExpressionNode Gt (IdentNode "x") (NumberNode 0.0)) (BlockNode [PrintStatementNode (IdentNode "x")]),"")]
while :: Parser StatementNode  -- ^ The parsed while loop statement
while = do
    token $ string "while"
    condition <- expression
    WhileNode condition <$> block

-- | Parses a block of statements enclosed in curly braces.
--
--
--   Example:
--
--   >>> parse block "{ var x = 5; print x; }"
--   [(BlockNode [VariableDeclarationNode "x" (NumberNode 5.0),PrintStatementNode (IdentNode "x")],"")]
block :: Parser StatementNode  -- ^ The parsed block statement
block = do
    token $ char '{'
    statements <- many statement
    token $ char '}'
    return $ BlockNode statements

-- | Parses an expression.
--
--   This parser is the entry point for parsing expressions.
--
--   Example:
--
--   >>> parse expression "x + 10 * (y - 5)"
--   [(BinaryExpressionNode Add (IdentNode "x") (BinaryExpressionNode Multiply (NumberNode 10.0) (BinaryExpressionNode Subtract (IdentNode "y") (NumberNode 5.0))),"")]
expression :: Parser ExpressionNode  -- ^ The parsed expression
expression = comparison

-- | Parses a comparison expression.
--
--   This parser parses a comparison expression, which consists of comparing two expressions using comparison operators such as '<', '>', '<=', '>=', '==', and '!='.
--
--   Example:
--
--   >>> parse comparison "x <= 10"
--   [(BinaryExpressionNode Lte (IdentNode "x") (NumberNode 10.0),"")]
comparison :: Parser ExpressionNode  -- ^ The parsed comparison expression
comparison = do
    left <- numeric
    comparison' left

comparison' :: ExpressionNode -> Parser ExpressionNode  -- ^ The parsed comparison expression with the left operand
comparison' left =
    do
        token $ char '<'
        right <- numeric
        comparison' (BinaryExpressionNode Lt left right)
        <|> do
            token $ string "<="
            right <- numeric
            comparison' (BinaryExpressionNode Lte left right)
        <|> do
            token $ string ">"
            right <- numeric
            comparison' (BinaryExpressionNode Gt left right)
        <|> do
            token $ string ">="
            right <- numeric
            comparison' (BinaryExpressionNode Gte left right)
        <|> do
            token $ string "=="
            right <- numeric
            comparison' (BinaryExpressionNode Eq left right)
        <|> do
            token $ string "!="
            right <- numeric
            comparison' (BinaryExpressionNode NotEq left right)
        <|> do
            token $ char '>'
            right <- numeric
            comparison' (BinaryExpressionNode NotEq left right)
        <|> return left

-- | Parses a numeric expression.
--
--   This parser parses a numeric expression, which consists of addition and subtraction of numeric values.
--
--   Example:
--
--   >>> parse numeric "x + 10 - y"
--   [(BinaryExpressionNode Subtract (BinaryExpressionNode Add (IdentNode "x") (NumberNode 10.0)) (IdentNode "y"),"")]
numeric :: Parser ExpressionNode  -- ^ The parsed numeric expression
numeric = do
    left <- term
    numeric' left

numeric' :: ExpressionNode -> Parser ExpressionNode  -- ^ The parsed numeric expression with the left operand
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

-- | Parses a term expression.
--
--   This parser parses a term, which consists of multiplication, division, and modulo operations.
--
--   Example:
--
--   >>> parse term "x * y / 10"
--   [(BinaryExpressionNode Divide (BinaryExpressionNode Multiply (IdentNode "x") (IdentNode "y")) (NumberNode 10.0),"")]
term :: Parser ExpressionNode  -- ^ The parsed term expression
term = do
    left <- signedFactor
    term' left

term' :: ExpressionNode -> Parser ExpressionNode  -- ^ The parsed term expression with the left operand
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

-- | Parses a signed factor in a numeric expression.
--
--   Example:
--
--   >>> parse signedFactor "-x"
--   [(UnaryExpressionNode Subtract (IdentNode "x"),"")]
signedFactor :: Parser ExpressionNode  -- ^ The parsed signed factor expression
signedFactor =
    do
        token $ char '-'
        UnaryExpressionNode Subtract <$> access
        <|> access

-- | Parses an array access expression.
--
--   Example:
--
--   >>> parse access "array[2]"
--   [(ArrayAccessNode (IdentNode "array") (NumberNode 2.0),"")]
access :: Parser ExpressionNode  -- ^ The parsed access expression
access =
    do
        left <- factor
        access' left
        <|> factor

access' :: ExpressionNode -> Parser ExpressionNode  -- ^ The parsed array access expression with the left operand
access' left =
    do
        token $ char '['
        index <- expression
        token $ char ']'
        access' (ArrayAccessNode left index)
        <|> return left

-- | Parses a factor expression.
--
--   This parser parses a factor expression, which can be a number, string, boolean, nil, identifier, parenthesized expression, or an array literal.
--
--   Example:
--
--   >>> parse factor "True"
--   [(BooleanNode True,"")]
factor :: Parser ExpressionNode  -- ^ The parsed factor expression
factor = number <|> string' <|> boolean <|> nil <|> identifier' <|> parenExpression <|> array

-- | Parses a numeric literal expression.
--
--   This parser parses a numeric literal expression, which can be an integer or floating-point number.
--
--   Example:
--
--   >>> parse number "42"
--   [(NumberNode 42.0,"")]
number :: Parser ExpressionNode  -- ^ The parsed numeric literal expression
number = float <|> integer'

-- | Parses an integer literal expression.
--
--   Example:
--
--   >>> parse integer' "42"
--   [(NumberNode 42.0,"")]
integer' :: Parser ExpressionNode  -- ^ The parsed integer literal expression
integer' = do
    n <- some digit
    return $ NumberNode $ read n

-- | Parses a floating-point literal expression.
--
--   Example:
--
--   >>> parse float "3.14"
--   [(NumberNode 3.14,"")]
float :: Parser ExpressionNode  -- ^ The parsed floating-point literal expression
float = do
    i <- some digit
    token $ char '.'
    f <- some digit
    return $ NumberNode $ read $ i ++ "." ++ f

-- | Parses a string literal expression.
--
--   Example:
--
--   >>> parse string' "\"Hello, World!\""
--   [(StringNode "Hello World!","")]
string' :: Parser ExpressionNode  -- ^ The parsed string literal expression
string' = do
    char '"'
    s <- many (sat (/= '"'))
    char '"'
    return $ StringNode s

-- | Parses a boolean literal expression.
--
--   This parser parses a boolean literal expression, which can be either "true" or "false".
--
--   Example:
--
--   >>> parse boolean "true"
--   [(BooleanNode True,"")]
boolean :: Parser ExpressionNode  -- ^ The parsed boolean literal expression
boolean = do
    b <- string "true" <|> string "false"
    return $ BooleanNode $ b == "true"

-- | Parses a nil literal expression.
--
--   This parser parses a nil literal expression, represented by the keyword "nil".
--
--   Example:
--
--   >>> parse nil "nil"
--   [(NilNode,"")]
nil :: Parser ExpressionNode  -- ^ The parsed nil literal expression
nil = do
    string "nil"
    return NilNode

-- | Parses an identifier expression.
--
--   Example:
--
--   >>> parse identifier' "x"
--   [(IdentNode "x","")]
identifier' :: Parser ExpressionNode  -- ^ The parsed identifier expression
identifier' = IdentNode <$> ident

-- | Parses a parenthesized expression.
--
--   This parser parses an expression enclosed in parentheses.
--
--   Example:
--
--   >>> parse parenExpression "(x + 2)"
--   [(BinaryExpressionNode Add (IdentNode "x") (NumberNode 2.0),"")]
parenExpression :: Parser ExpressionNode  -- ^ The parsed parenthesized expression
parenExpression = do
    token $ char '('
    expr <- expression
    token $ char ')'
    return expr

-- | Parses an array literal expression.
--
--   This parser parses an array literal expression, which consists of comma-separated expressions enclosed in square brackets.
--
--   Example:
--
--   >>> parse array "[1, 2, 3]"
--   [(ArrayNode [NumberNode 1.0,NumberNode 2.0,NumberNode 3.0],"")]
array :: Parser ExpressionNode  -- ^ The parsed array literal expression
array = do
    token $ char '['
    values <- expressionList
    token $ char ']'
    return $ ArrayNode values

-- | Parses a comma-separated list of expressions.
--
--   Example:
--
--   >>> parse expressionList "x, y, z"
--   [([IdentNode "x",IdentNode "y",IdentNode "z"],"")]
expressionList :: Parser [ExpressionNode]  -- ^ The parsed list of expressions
expressionList =
    many $
        do
            expr <- expression
            token $ char ','
            return expr
            <|> expression