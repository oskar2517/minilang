module Interpreter (execute, Context) where

import Data.Map (Map, empty, insert, lookup, member)
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
  negate (NumberObject x) = NumberObject $ -x
  abs (NumberObject x) = NumberObject $ abs x
  signum (NumberObject x) = NumberObject $ signum x
  fromInteger x = NumberObject $ fromInteger x

instance Fractional Object where
  (NumberObject x) / (NumberObject y) = NumberObject $ x / y
  recip (NumberObject x) = NumberObject $ recip x
  fromRational x = NumberObject $ fromRational x

instance Eq Object where
  (NumberObject x) == (NumberObject y) = x == y
  (BooleanObject x) == (BooleanObject y) = x == y
  _ == _ = False

instance Ord Object where
  compare (NumberObject x) (NumberObject y) = compare x y

mod' :: Object -> Object -> Object
mod' (NumberObject x) (NumberObject y) = NumberObject $ fromIntegral $ floor x `mod` floor y

data Context = Context {parent :: Maybe Context, variables :: Map String Object} deriving (Show)

findVariableContext :: String -> Maybe Context -> [Context] -> Maybe (Context, [Context])
findVariableContext name (Just context) traversed = if member name (variables context) then Just (context, traversed) else findVariableContext name (parent context) (context : traversed)
findVariableContext _ Nothing _ = Nothing

findVariable :: String -> Context -> Object
findVariable name context = case findVariableContext name (Just context) [] of
  Just (context, _) -> case Data.Map.lookup name $ variables context of
    Just o -> o
    Nothing -> error $ "<internal error>"
  Nothing -> error $ "Variable " ++ name ++ " not declared"

declareVariable :: String -> Object -> Context -> Context
declareVariable name value context = Context (parent context) (insert name value (variables context))

setVariable :: String -> Object -> Context -> Context
setVariable name value context = case findVariableContext name (Just context) [] of
  Just (context, traversed) -> do
    let newContext = Context (parent context) (insert name value (variables context))
    foldl (\context h -> Context (Just context) (variables h)) newContext (reverse traversed)
  Nothing -> error $ "Variable " ++ name ++ " not declared"

evalExpr :: Context -> ExpressionNode -> Object
evalExpr context (ArrayNode v) = ArrayObject (map (evalExpr context) v)
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
evalExpr context (BinaryExpressionNode Modulo left right) = mod' (evalExpr context left) (evalExpr context right)
evalExpr context (ArrayAccessNode target index) = case evalExpr context target of
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
          let context'' = executeStatement context body
          executeWhile context'' condition body
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
  context

executeStatement context (IfNode condition consequence alternative) = do
  context' <- context
  case evalExpr context' condition of
    BooleanObject b -> if b then executeStatement context consequence else executeStatement context alternative
    _ -> error "If condition must be of type boolean"

executeStatement context (WhileNode condition body) = do
  executeWhile context condition body

executeStatement context (VariableDeclarationNode name expr) = do
  context' <- context
  let value = evalExpr context' expr
  return $ declareVariable name value context'

executeStatement context (VariableAssignNode name expr) = do
  context' <- context
  let value = evalExpr context' expr
  return $ setVariable name value context'

execute :: StatementNode -> IO Context
execute = executeStatement (pure $ Context Nothing empty)
