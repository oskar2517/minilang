module Interpreter (execute, Object) where

import Data.Map (Map, empty, insert, lookup, member)
import Error (binaryOperatorError, genericError, internalError, unaryOperatorError)
import Parser (ExpressionNode (..), Operator (..), StatementNode (..))

data Object
    = NumberObject Float
    | StringObject String
    | ArrayObject [Object]
    | BooleanObject Bool
    | NilObject

data Context = Context {parent :: Maybe Context, variables :: Map String Object}
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

mod' :: Object -> Object -> Object
mod' (NumberObject x) (NumberObject y) =
    NumberObject $ fromIntegral $ floor x `mod` floor y
mod' left right = binaryOperatorError "%" (show left) (show right)

arrayReplace :: Int -> a -> [a] -> [a]
arrayReplace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

findVariableContext :: String -> Maybe Context -> [Context] -> Maybe (Context, [Context])
findVariableContext name (Just context) traversed =
    if member name (variables context)
        then Just (context, traversed)
        else findVariableContext name (parent context) (context : traversed)
findVariableContext _ Nothing _ = Nothing

findVariable :: String -> Context -> Object
findVariable name context = case findVariableContext name (Just context) [] of
    Just (context, _) -> case Data.Map.lookup name $ variables context of
        Just o -> o
        Nothing -> internalError $ "Variable " ++ name ++ " not found on context"
    Nothing -> genericError $ "Variable " ++ name ++ " does not exist"

declareVariable :: String -> Object -> Context -> Context
declareVariable name value context = Context (parent context) (insert name value (variables context))

setVariable :: String -> Object -> Context -> Context
setVariable name value context =
    case findVariableContext name (Just context) [] of
        Just (context, traversed) ->
            foldl
                (\context h -> Context (Just context) (variables h))
                (Context (parent context) (insert name value (variables context)))
                traversed
        Nothing -> genericError $ "Variable " ++ name ++ " has not been declared yet"

setArrayValue :: ExpressionNode -> Object -> Context -> Object
setArrayValue (ArrayAccessNode target index) value context = do
    let t = case evalExpr context target of
            ArrayObject v -> case evalExpr context index of
                NumberObject i -> ArrayObject $ arrayReplace (floor i) value v
                i -> genericError $ "Trying to index array with non-number '" ++ show i ++ "'"
            o -> genericError $ "Trying to index non-array '" ++ show o ++ "'"
    case target of
        ArrayAccessNode _ _ -> setArrayValue target t context
        _ -> t

variableName :: ExpressionNode -> String
variableName (ArrayAccessNode target _) = variableName target
variableName (IdentNode n) = n
variableName _ = internalError "Unable to find variable name of node"

evalExpr :: Context -> ExpressionNode -> Object
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

executeBlock :: IO Context -> [StatementNode] -> IO Context
executeBlock = foldl executeStatement

executeWhile :: IO Context -> ExpressionNode -> StatementNode -> IO Context
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

executeStatement :: IO Context -> StatementNode -> IO Context
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

execute :: StatementNode -> IO Context
execute = executeStatement (pure $ Context Nothing empty)
