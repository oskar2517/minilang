module Interpreter (execute, Object) where

import Data.Map (Map, empty, insert, lookup, member)
import Error (binaryOperatorError, genericError, internalError, unaryOperatorError)
import Parser (ExpressionNode (..), Operator (..), StatementNode (..))

-- | Represents the possible types of objects in the interpreter.
data Object
    = NumberObject Float
    | StringObject String
    | ArrayObject [Object]
    | BooleanObject Bool
    | NilObject

-- | Represents the context in which statements are executed.
data Context = Context
    { parent    :: Maybe Context  -- ^ The parent context, if any.
    , variables :: Map String Object -- ^ The variables defined in the context.
    }
    deriving (Show)

instance Num Object where
    (+) :: Object -> Object -> Object
    (NumberObject x) + (NumberObject y) = NumberObject $ x + y
    a + b = StringObject $ show a ++ show b

    (-) :: Object -> Object -> Object
    (NumberObject x) - (NumberObject y) = NumberObject $ x - y
    left - right = binaryOperatorError "-" (show left) (show right)

    (*) :: Object -> Object -> Object
    (NumberObject x) * (NumberObject y) = NumberObject $ x * y
    left * right = binaryOperatorError "*" (show left) (show right)

    negate :: Object -> Object
    negate (NumberObject x) = NumberObject $ -x
    negate right = unaryOperatorError "-" (show right)

    abs :: Object -> Object
    abs (NumberObject x) = NumberObject $ abs x

    signum :: Object -> Object
    signum (NumberObject x) = NumberObject $ signum x

    fromInteger :: Integer -> Object
    fromInteger x = NumberObject $ fromInteger x

instance Fractional Object where
    (/) :: Object -> Object -> Object
    (NumberObject x) / (NumberObject y) = NumberObject $ x / y
    left / right = binaryOperatorError "/" (show left) (show right)

    recip :: Object -> Object
    recip (NumberObject x) = NumberObject $ recip x

    fromRational :: Rational -> Object
    fromRational x = NumberObject $ fromRational x

instance Eq Object where
    (==) :: Object -> Object -> Bool
    (NumberObject x) == (NumberObject y) = x == y
    (StringObject x) == (StringObject y) = x == y
    (ArrayObject x) == (ArrayObject y) = x == y
    (BooleanObject x) == (BooleanObject y) = x == y
    NilObject == NilObject = True
    _ == _ = False

instance Ord Object where
    compare :: Object -> Object -> Ordering
    compare (NumberObject x) (NumberObject y) = compare x y
    compare left right = genericError $ "Cannot order " ++ show left ++ " and " ++ show right

instance Show Object where
    show :: Object -> String
    show (NumberObject n) =
        if n == fromInteger (round n)
            then show $ floor n
            else show n
    show (StringObject s) = s
    show (ArrayObject v) = show v
    show (BooleanObject b) =
        if b
            then "true"
            else "false"
    show NilObject = "nil"

-- | Utility function to perform modulo operation on two 'Object's. Throws an error whn either object is not of type 'NumberObject'.
mod' :: Object -- ^ The object to perform the modulo operation on 
     -> Object -- ^ The object to perform the modulo operation of
     -> Object -- ^ A new object containing the new value
mod' (NumberObject x) (NumberObject y) = NumberObject $ fromIntegral $ floor x `mod` floor y
mod' left right = binaryOperatorError "%" (show left) (show right)

-- | Utility function to replace an element at a specific position in a list.
--
--   The 'arrayReplace' function takes a position, a new value, and a list, and returns a new list with the element at the given position replaced by the new value.
arrayReplace :: Int -- ^ The index position to replace 
             -> a   -- ^ The value to replace it with
             -> [a] -- ^ The array to modify
             -> [a] -- ^ The modified array
arrayReplace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

-- | Utility function to find the context of a variable.
findVariableContext :: String                     -- ^ The name of the variable to find the context of
                    -> Maybe Context              -- ^ The context to start searching on 
                    -> [Context]                  -- ^ List of contexts that have been traversed searching for the variable
                    -> Maybe (Context, [Context]) -- ^ Either a Just of a tuple containing the context of the variable and all traversed contexts or Nothing.
findVariableContext name (Just context) traversed =
    if member name (variables context)
        then Just (context, traversed)
        else findVariableContext name (parent context) (context : traversed)
findVariableContext _ Nothing _ = Nothing

-- | Utility function to find the value of a variable in a context. Throws an error if the variable is not found.
findVariable :: String  -- ^ The name of the variable being searched 
             -> Context -- ^ The context to start searching on
             -> Object  -- ^ The value of the variable
findVariable name context = case findVariableContext name (Just context) [] of
    Just (context, _) -> case Data.Map.lookup name $ variables context of
        Just o -> o
        Nothing -> internalError $ "Variable " ++ name ++ " not found on context"
    Nothing -> genericError $ "Variable " ++ name ++ " does not exist"

-- | Utility function to declare a variable in a context.
declareVariable :: String  -- ^ The name of the variable to declare 
                -> Object  -- ^ The initial value to set the variable to
                -> Context -- ^ The context on which to declare the variable
                -> Context -- ^ A new context containing the variable
declareVariable name value context = Context (parent context) (insert name value (variables context))

-- | Utility function to set the value of a variable in a context. Throws an error if the variable has not been declared yet.
setVariable :: String  -- ^ The name of the variable to set
            -> Object  -- ^ The value to set the variable to
            -> Context -- ^ The starting context to start searching on
            -> Context -- ^ A new context containing the updated variable
setVariable name value context =
    case findVariableContext name (Just context) [] of
        Just (context, traversed) ->
            foldl
                (\context h -> Context (Just context) (variables h))
                (Context (parent context) (insert name value (variables context)))
                traversed
        Nothing -> genericError $ "Variable " ++ name ++ " has not been declared yet"

-- | Utility function to set the value of an element in an array.
setArrayValue :: ExpressionNode -- ^ The expression evaluating to an array to set the value of
              -> Object         -- ^ The value to set the array index to
              -> Context        -- ^ The context in which to evaluate the array expression
              -> Object         -- ^ The updated array
setArrayValue (ArrayAccessNode target index) value context = do
    let t = case evalExpr context target of
            ArrayObject v -> case evalExpr context index of
                NumberObject i -> ArrayObject $ arrayReplace (floor i) value v
                i -> genericError $ "Trying to index array with non-number '" ++ show i ++ "'"
            o -> genericError $ "Trying to index non-array '" ++ show o ++ "'"
    case target of
        ArrayAccessNode _ _ -> setArrayValue target t context
        _ -> t

-- | Utility function to extract the variable name from an 'ExpressionNode'. Used for array value assigning.
variableName :: ExpressionNode -- ^ The current searching node 
             -> String         -- ^ The name of the variable
variableName (ArrayAccessNode target _) = variableName target
variableName (IdentNode n) = n
variableName _ = internalError "Unable to find variable name of node"

-- | Evaluates an expression in the specified context and returns the resulting 'Object'.
evalExpr :: Context        -- ^ The context to evaluate the expression in 
         -> ExpressionNode -- ^ The expression to evaluate
         -> Object         -- ^ The object to which the expression has been evaluated
evalExpr context (ArrayNode v) = ArrayObject (map (evalExpr context) v)
evalExpr context (NumberNode n) = NumberObject n
evalExpr context (StringNode s) = StringObject s
evalExpr context (IdentNode n) = findVariable n context
evalExpr context (BooleanNode b) = BooleanObject b
evalExpr context NilNode = NilObject
evalExpr context (UnaryExpressionNode Subtract left) = -evalExpr context left
evalExpr context (BinaryExpressionNode Add left right) =
    evalExpr context left + evalExpr context right
evalExpr context (BinaryExpressionNode Subtract left right) =
    evalExpr context left - evalExpr context right
evalExpr context (BinaryExpressionNode Multiply left right) =
    evalExpr context left * evalExpr context right
evalExpr context (BinaryExpressionNode Divide left right) =
    evalExpr context left / evalExpr context right
evalExpr context (BinaryExpressionNode Eq left right) =
    BooleanObject $ evalExpr context left == evalExpr context right
evalExpr context (BinaryExpressionNode NotEq left right) =
    BooleanObject $ evalExpr context left /= evalExpr context right
evalExpr context (BinaryExpressionNode Lt left right) =
    BooleanObject $ evalExpr context left < evalExpr context right
evalExpr context (BinaryExpressionNode Lte left right) =
    BooleanObject $ evalExpr context left <= evalExpr context right
evalExpr context (BinaryExpressionNode Gt left right) =
    BooleanObject $ evalExpr context left > evalExpr context right
evalExpr context (BinaryExpressionNode Gte left right) =
    BooleanObject $ evalExpr context left >= evalExpr context right
evalExpr context (BinaryExpressionNode Modulo left right) =
    mod' (evalExpr context left) (evalExpr context right)
evalExpr context (ArrayAccessNode target index) =
    case evalExpr context target of
        ArrayObject v -> do
            case evalExpr context index of
                NumberObject i -> v !! floor i
                i -> genericError $ "Trying to index array with non-number '" ++ show i ++ "'"
        o -> genericError $ "Trying to index non-array '" ++ show o ++ "'"

-- | Executes a block node by executing all statements inside of it.
executeBlock :: IO Context        -- ^ The context to execute the block in 
               -> [StatementNode] -- ^ The statements to execute
               -> IO Context      -- ^ A new context containing all changes made by the statements that have been executed
executeBlock = foldl executeStatement

-- | Executes a while statements. Throws an error if condition does not evaluate to a boolean value.
executeWhile :: IO Context     -- ^ The context to execute the while statement in 
             -> ExpressionNode -- ^ The condition of the while statement
             -> StatementNode  -- ^ The body of the while statements
             -> IO Context     -- A new context containing all changes made by the statements that have been executed
executeWhile context condition body = do
    context' <- context
    case evalExpr context' condition of
        BooleanObject b ->
            if b
                then do
                    context'' <- executeStatement context body
                    executeWhile (pure context'') condition body
                else context
        _ -> genericError "While condition must be of type boolean"

-- | Executes a single statement.
executeStatement :: IO Context    -- ^ The context to execute the statement in 
                 -> StatementNode -- ^ The statement to be executed
                 -> IO Context    -- ^ A new context containing all changes made by the statements that have been executed
executeStatement context (BlockNode n) = do
    context' <- context
    newContext <- executeBlock (pure $ Context (Just context') empty) n
    case parent newContext of
        Just c -> pure c
        Nothing -> internalError "Block context has no parent"
executeStatement context (PrintStatementNode e) = do
    context' <- context
    print $ evalExpr context' e
    return context'
executeStatement context (IfNode condition consequence alternative) = do
    context' <- context
    case evalExpr context' condition of
        BooleanObject b ->
            if b
                then executeStatement (pure context') consequence
                else executeStatement (pure context') alternative
        _ -> genericError "If condition must be of type boolean"
executeStatement context (WhileNode condition body) = do
    context' <- context
    executeWhile (pure context') condition body
executeStatement context (VariableDeclarationNode name expr) = do
    context' <- context
    let value = evalExpr context' expr
    return $ declareVariable name value context'
executeStatement context (VariableAssignNode name expr) = do
    context' <- context
    let value = evalExpr context' expr
    return $ setVariable name value context'
executeStatement context (ArrayAssignNode target value) = do
    context' <- context
    let newVal = setArrayValue target (evalExpr context' value) context'
    let varName = variableName target
    return $ setVariable varName newVal context'

-- | Executes a single statement in a new context. Used to execute the entire program.
execute :: StatementNode -- ^ The statement to be executed 
        -> IO Context    -- ^ The last context used during execution
execute = executeStatement (pure $ Context Nothing empty)
