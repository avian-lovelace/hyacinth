module ParserSpec () where
--   testParser
-- ) where

-- import Test.Hspec

-- import TokenTestHelpers
-- import Parser
-- import Errors
-- import Core

-- import Data.Sequence(fromList)

-- testParser :: Spec
-- testParser = do
--   describe "expressionParser" $ do
--     -- Test single expressions
--     it "parses numbers" $ let
--       input = fromList [oneToken]
--       expectedExpression = oneExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "parses negative numbes" $ let
--       input = fromList [minusToken, oneToken]
--       expectedExpression = NegateExpression oneExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "parses addition" $ let
--       input = fromList [oneToken, plusToken, twoToken]
--       expectedExpression = AddExpression oneExpression twoExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "parses subtraction" $ let
--       input = fromList [oneToken, minusToken, twoToken]
--       expectedExpression = SubtractExpression oneExpression twoExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "parses multiplication" $ let
--       input = fromList [oneToken, starToken, twoToken]
--       expectedExpression = MultiplyExpression oneExpression twoExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "parses division" $ let
--       input = fromList [oneToken, slashToken, twoToken]
--       expectedExpression = DivideExpression oneExpression twoExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression

--     it "parses parentheses" $ let
--       input = fromList [leftParenToken, oneToken, rightParenToken]
--       expectedExpression = oneExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     -- Test multiple of the same expression
--     it "errors on multiple negates" $ let
--       input = fromList [minusToken, minusToken, oneToken]
--       in (expressionParser $ makeStream input) `shouldSatisfy` matchError

--     it "left-associates addition and subtraction" $ let
--       input = fromList [oneToken, plusToken, twoToken, minusToken, threeToken, plusToken, fourToken]
--       expectedExpression = ((oneExpression `AddExpression` twoExpression)  `SubtractExpression` threeExpression) `AddExpression` fourExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     it "left-associates multiplication and division" $ let
--       input = fromList [oneToken, starToken, twoToken, slashToken, threeToken, starToken, fourToken]
--       expectedExpression = ((oneExpression `MultiplyExpression` twoExpression) `DivideExpression` threeExpression) `MultiplyExpression` fourExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression

--     -- Test operator precedence
--     it "groups multiplication over addition" $ let
--       input = fromList [oneToken, starToken, twoToken, plusToken, threeToken, starToken, fourToken]
--       expectedExpression = AddExpression (MultiplyExpression oneExpression twoExpression) (MultiplyExpression threeExpression fourExpression)
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression

--     it "groups negation over multiplication" $ let
--       input = fromList [minusToken, oneToken, starToken, minusToken, twoToken]
--       expectedExpression = MultiplyExpression (NegateExpression oneExpression) (NegateExpression twoExpression)
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression
    
--     -- Test parentheses
--     it "respects parentheses" $ let
--       input = fromList [oneToken, starToken, leftParenToken, twoToken, plusToken, threeToken, rightParenToken, starToken, fourToken]
--       expectedExpression = (oneExpression `MultiplyExpression` (twoExpression `AddExpression` threeExpression)) `MultiplyExpression` fourExpression
--       actualExpression = snd <$> (expressionParser $ makeStream input)
--       in actualExpression `shouldBe` Success expectedExpression

--     -- Test assignables
--     it "parses single identifiers as variables" $ let
--       input = fromList [id1Token]
--       actualExpression = snd <$> (assignableParser $ makeStream input)
--       expectedExpression = (Variable "id1")
--       in actualExpression `shouldBeNL` (Success expectedExpression)

--     it "parses mutliple identifiers as a contructor" $ let
--       input = fromList [id1Token, id2Token, id3Token]
--       actualExpression = snd <$> (assignableParser $ makeStream input)
--       expectedExpression = (Constructor "id1" [Variable "id2", Variable "id3"])
--       in actualExpression `shouldBeNL` (Success expectedExpression)
    
--     it "parses parenthesized assignables" $ let
--       input = fromList [leftParenToken, id1Token, id2Token, id3Token, rightParenToken]
--       actualExpression = snd <$> (assignableParser $ makeStream input)
--       expectedExpression = (Constructor "id1" [Variable "id2", Variable "id3"])
--       in actualExpression `shouldBeNL` (Success expectedExpression)
    
--     it "respected parenthesized subassignables" $ let
--       input = fromList [id1Token, leftParenToken, id2Token, id3Token, rightParenToken, id4Token]
--       actualExpression = snd <$> (assignableParser $ makeStream input)
--       expectedExpression = (Constructor "id1" [Constructor "id2" [Variable "id3"], Variable "id4"])
--       in actualExpression `shouldBeNL` (Success expectedExpression)
    
--     -- Test statements
--     it "parses return expressions" $ let
--       input = fromList [returnToken, oneToken, semicolonToken]
--       actualExpression = snd <$> (statementParser $ makeStream input)
--       expectedExpression = (ReturnStatement $ IntExpression 1)
--       in actualExpression `shouldBeNL` (Success expectedExpression)
    
--     it "parses assignment expressions" $ let
--       input = fromList [id1Token, equalsToken, oneToken, semicolonToken]
--       actualExpression = snd <$> (statementParser $ makeStream input)
--       expectedExpression = (AssignmentStatement (Variable "id1") (IntExpression 1))
--       in actualExpression `shouldBeNL` (Success expectedExpression)

-- oneExpression :: Expression
-- oneExpression = IntExpression 1
-- twoExpression :: Expression
-- twoExpression = IntExpression 2
-- threeExpression :: Expression
-- threeExpression = IntExpression 3
-- fourExpression :: Expression
-- fourExpression = IntExpression 4


-- matchError :: WithError a -> Bool
-- matchError withError = case withError of
--   Error _ -> True
--   Success _ -> False