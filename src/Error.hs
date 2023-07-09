module Error where

-- | Throws a generic error with a given message.
--
--   This function is used to signal errors during runtime.
--
--   Example:
--
--   >>> genericError "Something went wrong!"
--   *** Exception: [Error] Something went wrong!
genericError :: String  -- ^ The error message
             -> a       -- ^ The result type (since it throws an error, it can be any type)
genericError s = error $ "[Error] " ++ s

-- | Throws an error indicating that a binary operator is not supported for the given operands.
--
--   This function is used to signal errors related to unsupported binary operators.
--
--   Example:
--
--   >>> binaryOperatorError "+" "1" "True"
--   *** Exception: [Error] Binary operator + not supported for 1 and True
binaryOperatorError :: String  -- ^ The operator
                    -> String  -- ^ The left operand
                    -> String  -- ^ The right operand
                    -> a       -- ^ The result type (since it throws an error, it can be any type)
binaryOperatorError operator left right = genericError $ "Binary operator " ++ operator ++ " not supported for " ++ left ++ " and " ++ right

-- | Throws an error indicating that a unary operator is not supported for the given operand.
--
--   This function is used to signal errors related to unsupported unary operators.
--
--   Example:
--
--   >>> unaryOperatorError "-" "True"
--   *** Exception: [Error] Unary operator - not supported for True
unaryOperatorError :: String  -- ^ The operator
                   -> String  -- ^ The operand
                   -> a       -- ^ The result type (since it throws an error, it can be any type)
unaryOperatorError operator right = genericError $ "Unary operator " ++ operator ++ " not supported for " ++ right

-- | Throws an error indicating an internal interpreter error.
--
--   This function is used to signal internal errors that occur during interpretation.
--
--   Example:
--
--   >>> internalError "Undefined variable: x"
--   *** Exception: [Error] <internal> This is an interpreter error! Undefined variable: x
internalError :: String  -- ^ The error message
              -> a       -- ^ The result type (since it throws an error, it can be any type)
internalError s = genericError $ "<internal> This is an interpreter error! " ++ s
