module Interpreter (execute, Context) where

import Data.Map (Map, empty, lookup, member, insert)
import Parser (ExpressionNode (..), Operator (..), StatementNode (..))

data Object
  = NumberObject Float
  | StringObject String
  | ArrayObject [Object]
  | BooleanObject Bool
  | NilObject
  deriving (Show)

instance Num Object where
  (NumberObject x) + (NumberObject y) = NumberObject $ x + y
  (NumberObject x) - (NumberObject y) = NumberObject $ x - y
  (NumberObject x) * (NumberObject y) = NumberObject $ x * y

instance Fractional Object where
  (NumberObject x) / (NumberObject y) = NumberObject $ x / y

instance Eq Object where
  (NumberObject x) == (NumberObject y) = x == y
  (BooleanObject x) == (BooleanObject y) = x == y
  _ == _ = False

instance Ord Object where
  compare (NumberObject x) (NumberObject y) = compare x y

data Context = Context {parent :: Maybe Context, variables :: Map String Object} deriving (Show)

findVariableContext :: String -> Maybe Context -> Maybe Context
findVariableContext name (Just context) = if member name (variables context) then Just context else findVariableContext name $ parent context
findVariableContext _ Nothing = Nothing

findVariable :: String -> Context -> Object
findVariable name context = case findVariableContext name (Just context) of
    Just context -> case Data.Map.lookup name $ variables context of
        Just o -> o
        Nothing -> error "Variable not found"
    Nothing -> error "Variable not found"

setVariable' :: String -> Object -> Maybe Context -> [Context] -> Maybe Context
setVariable' name value (Just context) traversed = do
    if member name (variables context) then do
        let newContext = Context (parent context) (insert name value (variables context))
        Just $ foldl (\ context h -> Context (Just context) (variables h)) newContext (reverse traversed)
    else setVariable' name value (parent context) (context : traversed)
setVariable' _ _ Nothing _ = Nothing

setVariable'' :: String -> Object -> Context -> Context
setVariable'' name value context = case setVariable' name value (Just context) [] of
    Just c -> c
    Nothing -> Context (parent context) (insert name value (variables context))

evalExpr :: Context -> ExpressionNode -> Object
evalExpr context (NumberNode n) = NumberObject n
evalExpr context (StringNode s) = StringObject s
evalExpr context (IdentNode n) = findVariable n context
evalExpr context (BooleanNode b) = BooleanObject b
evalExpr context NilNode = NilObject
evalExpr context (UnaryExpressionNode Subtract left) = -evalExpr context left
evalExpr context (BinaryExpressionNode Add left right) = evalExpr context left + evalExpr context right
evalExpr context (BinaryExpressionNode Subtract left right) = evalExpr context left - evalExpr context right
evalExpr context (BinaryExpressionNode Multiply left right) = evalExpr context left * evalExpr context right
evalExpr context (BinaryExpressionNode Divide left right) = evalExpr context left / evalExpr context right
evalExpr context (BinaryExpressionNode Eq left right) = BooleanObject $ evalExpr context left == evalExpr context right
evalExpr context (BinaryExpressionNode NotEq left right) = BooleanObject $ evalExpr context left /= evalExpr context right
evalExpr context (BinaryExpressionNode Lt left right) = BooleanObject $ evalExpr context left < evalExpr context right
evalExpr context (BinaryExpressionNode Lte left right) = BooleanObject $ evalExpr context left <= evalExpr context right
evalExpr context (BinaryExpressionNode Gt left right) = BooleanObject $ evalExpr context left > evalExpr context right
evalExpr context (BinaryExpressionNode Gte left right) = BooleanObject $ evalExpr context left >= evalExpr context right

executeBlock :: IO Context -> [StatementNode] -> IO Context
executeBlock context [] = context
executeBlock context (h:t) = do
    let newContext = executeStatement context h
    executeBlock newContext t

executeStatement :: IO Context -> StatementNode -> IO Context
executeStatement context (BlockNode n) = do
    con <- context
    con2 <- executeBlock (pure $ Context (Just con) empty) n
    case parent con2 of
        Just c -> pure c
        Nothing -> pure con
executeStatement context (PrintStatementNode e) = do
    con <- context
    print $ evalExpr con e
    context
executeStatement context (IfNode condition consequence alternative) = do
    con <- context
    if evalExpr con condition == BooleanObject True
        then executeStatement context consequence
        else executeStatement context alternative
executeStatement context (VariableAssignNode name expr) = do
    con <- context
    let value = evalExpr con expr
    return $ setVariable'' name value con

execute :: StatementNode -> IO Context
execute = executeStatement (pure $ Context Nothing empty)
