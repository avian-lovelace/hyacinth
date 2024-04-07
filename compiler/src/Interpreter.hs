module Interpreter () where

--   ( evaluateExpression
--   )
-- where

-- import Parser

-- data Value = IntValue Integer
--   deriving (Eq, Show)

-- type WithRuntimeError a = Either String a

-- evaluateExpression :: Expression -> WithRuntimeError Value
-- evaluateExpression (IntExpression value) = Right $ IntValue value
-- evaluateExpression (AddExpression left right) = do
--   leftValue <- evaluateExpression left >>= takeIntValue
--   rightValue <- evaluateExpression right >>= takeIntValue
--   return $ IntValue (leftValue + rightValue)
-- evaluateExpression (SubtractExpression left right) = do
--   leftValue <- evaluateExpression left >>= takeIntValue
--   rightValue <- evaluateExpression right >>= takeIntValue
--   return $ IntValue (leftValue - rightValue)
-- evaluateExpression (MultiplyExpression left right) = do
--   leftValue <- evaluateExpression left >>= takeIntValue
--   rightValue <- evaluateExpression right >>= takeIntValue
--   return $ IntValue (leftValue * rightValue)
-- evaluateExpression (DivideExpression left right) = do
--   leftValue <- evaluateExpression left >>= takeIntValue
--   rightValue <- evaluateExpression right >>= takeIntValue
--   return $ IntValue (leftValue `div` rightValue)
-- evaluateExpression (NegateExpression inner) = do
--   innerValue <- evaluateExpression inner >>= takeIntValue
--   return $ IntValue (-innerValue)

-- takeIntValue :: Value -> WithRuntimeError Integer
-- takeIntValue value = case value of
--   IntValue int -> Right int
--   -- _ -> Left "expected integer but got _"