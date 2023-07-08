module Interpreter (execute) where

import Data.Map (Map, empty, insert, lookup, member)
import Parser (ExpressionNode (..), Operator (..), StatementNode (..))

data Object
    = NumberObject Float
    | StringObject String
    | ArrayObject [Object]
    | BooleanObject Bool
    | NilObject

instance Num Object where
    (+) :: Object -> Object -> Object
    (NumberObject x) + (NumberObject y) = NumberObject $ x + y
    (-) :: Object -> Object -> Object
    (NumberObject x) - (NumberObject y) = NumberObject $ x - y
    (*) :: Object -> Object -> Object
    (NumberObject x) * (NumberObject y) = NumberObject $ x * y
    negate :: Object -> Object
    negate (NumberObject x) = NumberObject $ -x
    abs :: Object -> Object
    abs (NumberObject x) = NumberObject $ abs x
    signum :: Object -> Object
    fromInteger :: Integer -> Object
    signum (NumberObject x) = NumberObject $ signum x
    fromInteger x = NumberObject $ fromInteger x

instance Fractional Object where
    (/) :: Object -> Object -> Object
    (NumberObject x) / (NumberObject y) = NumberObject $ x / y
    recip :: Object -> Object
    recip (NumberObject x) = NumberObject $ recip x
    fromRational :: Rational -> Object
    fromRational x = NumberObject $ fromRational x

instance Eq Object where
    (NumberObject x) == (NumberObject y) = x == y
    (BooleanObject x) == (BooleanObject y) = x == y
    _ == _ = False

instance Ord Object where
    compare :: Object -> Object -> Ordering
    compare (NumberObject x) (NumberObject y) = compare x y

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

data Context = Context {parent :: Maybe Context, variables :: Map String Object}
    deriving (Show)

findVariableContext ::
    String -> Maybe Context -> [Context] -> Maybe (Context, [Context])
findVariableContext name (Just context) traversed =
    if member name (variables context)
        then Just (context, traversed)
        else findVariableContext name (parent context) (context : traversed)
findVariableContext _ Nothing _ = Nothing

findVariable :: String -> Context -> Object
findVariable name context = case findVariableContext name (Just context) [] of
    Just (context, _) -> case Data.Map.lookup name $ variables context of
        Just o -> o
        Nothing -> error $ "<internal error>"
    Nothing -> error $ "Variable " ++ name ++ " not declared"

declareVariable :: String -> Object -> Context -> Context
declareVariable name value context =
    Context (parent context) (insert name value (variables context))

setVariable :: String -> Object -> Context -> Context
setVariable name value context =
    case findVariableContext name (Just context) [] of
        Just (context, traversed) -> do
            let newContext =
                    Context (parent context) (insert name value (variables context))
            foldl
                (\context h -> Context (Just context) (variables h))
                newContext
                traversed
        Nothing -> error $ "Variable " ++ name ++ " not declared"

replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

-- todo: testen ob index int ist
setArrayValue :: ExpressionNode -> Object -> Context -> Object
setArrayValue (ArrayAccessNode target index) value context = do
    let t = case evalExpr context target of
            ArrayObject v -> case evalExpr context index of
                NumberObject i -> ArrayObject $ replace (floor i) value v
                _ -> error ""
            _ -> error ""
    case target of
        ArrayAccessNode _ _ -> setArrayValue target t context
        _ -> t

variableName :: ExpressionNode -> String
variableName (ArrayAccessNode target _) = variableName target
variableName (IdentNode n) = n

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
                _ -> error "Array index must be a number"
        _ -> error "Trying to index non-array"

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
        _ -> error "While condition must be of type boolean"

executeStatement :: IO Context -> StatementNode -> IO Context
executeStatement context (BlockNode n) = do
    context' <- context
    context'' <- executeBlock (pure $ Context (Just context') empty) n
    case parent context'' of
        Just c -> pure c
        Nothing -> error "<internal error>"
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
        _ -> error "If condition must be of type boolean"
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
