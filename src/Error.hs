module Error where

genericError :: String -> a
genericError s = error $ "[Error] " ++ s

binaryOperatorError :: String -> String -> String -> a
binaryOperatorError operator left right = genericError $ "Binary operator " ++ operator ++ " not supported for " ++ left ++ " and " ++ right

unaryOperatorError :: String -> String -> a
unaryOperatorError operator right = genericError $ "Unary operator " ++ operator ++ " not supported for " ++ right

internalError :: String -> a
internalError s = genericError $ "<internal> This is an interpreter error! " ++ s