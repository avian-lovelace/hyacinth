module Parsing.Parser
  ( parseFile,
  )
where

import Control.Applicative
import Control.Monad (foldM)
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import qualified Data.Sequence as Seq
import Lexing.Tokens
import Parsing.Parsing
import Parsing.SyntaxTree
import Sectioning.Sectioning

parseFile :: ParseFunction PModule
parseFile sections = Module () . MainFunction () <$> parseScope sections

parseScope :: ParseFunction PScope
parseScope sections = do
  combinedStatements <- consolidateErrors $ parseStatementsHelper sections
  let (statements, nonPositionalStatements) = seqPartitionEither combinedStatements
  return $ Scope () nonPositionalStatements statements
  where
    parseStatementsHelper :: Seq Section -> Seq (WithErrors (Either PStatement PNonPositionalStatement))
    parseStatementsHelper Empty = Empty
    parseStatementsHelper helperSections = case Seq.breakl (isJust . matchSemicolon) helperSections of
      (statementSections, Empty) -> Seq.singleton $ Error [ExpectedToEndWithSemicolonError $ getRange statementSections]
      (statementSections, _semicolon :<| restTokens) -> parseStatement statementSections <| parseStatementsHelper restTokens

matchSemicolon :: Section -> Maybe ()
matchSemicolon (TokenSection (SemicolonToken _)) = Just ()
matchSemicolon _ = Nothing

-- Statements

parseStatement :: ParseFunction (Either PStatement PNonPositionalStatement)
parseStatement Empty = singleError $ ShouldNotGetHereError "To be implemented"
parseStatement (currentSection :<| tailSections) = case currentSection of
  TokenSection (PrintToken _) -> Left <$> parsePrintStatement currentSection tailSections
  TokenSection (LetToken _) -> Left <$> parseVariableDeclarationStatement currentSection tailSections
  TokenSection (MutToken _) -> Left <$> parseVariableMutationStatement currentSection tailSections
  TokenSection (WhileToken _) -> Left <$> parseWhileLoopStatement currentSection tailSections
  TokenSection (ReturnToken _) -> Left <$> parseReturnStatement currentSection tailSections
  TokenSection (FuncToken _) -> Right <$> parseFunctionStatement currentSection tailSections
  TokenSection (RecToken _) -> Right <$> parseRecordStatement currentSection tailSections
  _ -> Left <$> parseExpressionStatement (currentSection <| tailSections)

parsePrintStatement :: Section -> ParseFunction PStatement
parsePrintStatement printTokenSection expressionSections = case expressionSections of
  Empty -> singleError $ PrintStatementEmptyExpressionError $ getRange printTokenSection
  _ -> PrintStatement statementRange <$> expressionOrErrors
    where
      statementRange = getRange (printTokenSection, seqTail expressionSections)
      expressionRange = getRange expressionSections
      expressionOrErrors = catchUnboundError (PrintStatementInvalidExpressionError expressionRange) $ runParserToEnd expressionParser expressionSections

parseVariableDeclarationStatement :: Section -> ParseFunction PStatement
parseVariableDeclarationStatement letTokenSection restSections1 = do
  (mutability, restSections2) <- case restSections1 of
    ((TokenSection (MutToken _)) :<| restSections2) -> return (Mutable, restSections2)
    _ -> return (Immutable, restSections1)
  (variableName, restSections3) <- case restSections2 of
    (TokenSection (IdentifierToken _ identifierName)) :<| restSections3 -> return (identifierName, restSections3)
    _ -> singleError $ VariableDeclarationMalformedError statementRange
  (maybeVariableTypeSections, valueSections) <- case restSections3 of
    (TokenSection (ColonToken _)) :<| restSections4 -> do
      (typeSections, valueSections) <- case Seq.breakl isEqualsSection restSections4 of
        (_, Empty) -> singleError $ VariableDeclarationMalformedError statementRange
        (typeSections, _equalsSection :<| valueSections) -> return (typeSections, valueSections)
      return (Just typeSections, valueSections)
    (TokenSection (EqualsToken _)) :<| valueSections -> return (Nothing, valueSections)
    _ -> singleError $ VariableDeclarationMalformedError statementRange
  variableType <- case maybeVariableTypeSections of
    Nothing -> return Nothing
    Just Empty -> singleError $ VariableDeclarationEmptyTypeError statementRange
    Just variableTypeSections ->
      catchUnboundError (VariableDeclarationMalformedTypeError (getRange valueSections)) $
        Just <$> runParserToEnd typeExpressionParser variableTypeSections
  variableValue <- case valueSections of
    Empty -> singleError $ VariableDeclarationEmptyExpressionError statementRange
    _ -> catchUnboundError (VariableDeclarationInvalidExpressionError (getRange valueSections)) $ runParserToEnd expressionParser valueSections
  return $ VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName variableType) variableValue
  where
    statementRange = case restSections1 of
      Empty -> getRange letTokenSection
      _ -> getRange (letTokenSection, restSections1)

isEqualsSection :: Section -> Bool
isEqualsSection (TokenSection (EqualsToken _)) = True
isEqualsSection _ = False

parseVariableMutationStatement :: Section -> ParseFunction PStatement
parseVariableMutationStatement
  mutTokenSection
  ((TokenSection (IdentifierToken _ _)) :<| (TokenSection (EqualsToken equalsRange)) :<| Empty) =
    singleError $ VariableMutationEmptyExpressionError $ getRange (mutTokenSection, equalsRange)
parseVariableMutationStatement
  mutTokenSection
  ((TokenSection (IdentifierToken _ identifierName)) :<| (TokenSection (EqualsToken _)) :<| expressionSections) =
    VariableMutationStatement statementRange identifierName <$> expression
    where
      statementRange = getRange (mutTokenSection, seqTail expressionSections)
      expression = catchUnboundError (VariableMutationInvalidExpressionError expressionRange) $ runParserToEnd expressionParser expressionSections
      expressionRange = getRange expressionSections
parseVariableMutationStatement mutTokenSection restSections = singleError $ VariableDeclarationMalformedError (getRange (mutTokenSection :<| restSections))

parseExpressionStatement :: ParseFunction PStatement
parseExpressionStatement expressionSections = ExpressionStatement expressionRange <$> expression
  where
    expression = catchUnboundError (ExpressionStatementInvalidExpressionError expressionRange) $ runParserToEnd expressionParser expressionSections
    expressionRange = getRange expressionSections

parseWhileLoopStatement :: Section -> ParseFunction PStatement
parseWhileLoopStatement whileTokenSection restSections = case Seq.breakl matchLoopTokenSection restSections of
  (Empty, Empty) -> singleError $ WhileStatementNoLoopError $ getRange whileTokenSection
  (_, Empty) -> singleError $ WhileStatementNoLoopError whileStatementRange
  (Empty, _) -> singleError $ WhileStatementEmptyConditionError whileStatementRange
  (_, _loopTokenSection :<| Empty) -> singleError $ WhileStatementEmptyStatementError whileStatementRange
  (conditionSections, _loopTokenSection :<| bodySections) -> do
    condition <-
      catchUnboundError (WhileStatementMalformedConditionExpressionError $ getRange conditionSections) $
        runParserToEnd expressionParser conditionSections
    body <-
      catchUnboundError (WhileStatementMalformedBodyExpressionError $ getRange bodySections) $
        runParserToEnd expressionParser bodySections
    return $ WhileLoopStatement whileStatementRange condition body
  where
    matchLoopTokenSection (TokenSection (LoopToken _)) = True
    matchLoopTokenSection _ = False
    -- Must check that restSections is non-empty before using
    whileStatementRange = getRange (whileTokenSection, seqTail restSections)

parseReturnStatement :: Section -> ParseFunction PStatement
parseReturnStatement returnTokenSection expressionSections = case expressionSections of
  Empty -> Success $ ReturnStatement (getRange returnTokenSection) Nothing
  _ -> ReturnStatement statementRange . Just <$> expressionOrErrors
    where
      statementRange = getRange (returnTokenSection, seqTail expressionSections)
      expressionRange = getRange expressionSections
      expressionOrErrors = catchUnboundError (ReturnStatementInvalidExpressionError expressionRange) $ runParserToEnd expressionParser expressionSections

parseFunctionStatement :: Section -> ParseFunction PNonPositionalStatement
parseFunctionStatement funcTokenSection restSections1 = do
  (functionName, parameters, restSections2) <- case restSections1 of
    ((TokenSection (IdentifierToken _ functionName)) :<| (SquareBracketSection parameterListRange parameterListSections) :<| restSections2) -> do
      parameters <- parseParamterList parameterListRange parameterListSections
      return (functionName, parameters, restSections2)
    _ -> singleError $ FunctionStatementMalformedError statementRange
  (returnTypeAnnotation, bodySections) <- case restSections2 of
    {- Breaking up the type and body on the leftmost right arrow does mean that if the return type is a function, it may
      need to be put in parentheses to parse successfully. We could try to be smarter about breaking on the correct
      right arrow, but I don't think it's worth it. In these cases, it's probably good for code readability to put the
      type in parentheses anyways.
    -}
    (TokenSection (ColonToken _)) :<| restSections3 -> case Seq.breakl isSingleRightArrowSection restSections3 of
      (_, Empty) -> singleError $ FunctionStatementMalformedReturnTypeError statementRange
      (returnTypeSections, _arrow :<| bodySections) -> do
        returnType <- catchUnboundError (FunctionStatementMalformedReturnTypeError (getRange returnTypeSections)) $ runParserToEnd typeExpressionParser returnTypeSections
        return (Just returnType, bodySections)
    (TokenSection (SingleRightArrowToken _) :<| bodySections) -> return (Nothing, bodySections)
    _ -> singleError $ FunctionStatementMalformedError statementRange
  body <- case bodySections of
    Empty -> singleError $ FunctionStatementEmptyBodyError statementRange
    _ -> catchUnboundError (FunctionStatementMalformedBodyError (getRange bodySections)) $ runParserToEnd expressionParser bodySections
  return $ FunctionStatement statementRange functionName (FunctionDefinition statementRange parameters (WithTypeAnnotation body returnTypeAnnotation))
  where
    statementRange = case restSections1 of
      Empty -> getRange funcTokenSection
      _ -> getRange (funcTokenSection, restSections1)

isSingleRightArrowSection :: Section -> Bool
isSingleRightArrowSection (TokenSection (SingleRightArrowToken _)) = True
isSingleRightArrowSection _ = False

parseRecordStatement :: Section -> ParseFunction PNonPositionalStatement
parseRecordStatement
  recTokenSection
  ( (TokenSection (IdentifierToken _ recordName))
      :<| (SquareBracketSection fieldTypeListRange fieldTypeListSections)
      :<| Empty
    ) = do
    let fieldSectionLists = seqSplitOn isCommaSection fieldTypeListSections
    fieldTypePairs <- consolidateErrors $ parseFieldType <$> fieldSectionLists
    return $ RecordStatement (getRange recTokenSection <> fieldTypeListRange) recordName fieldTypePairs
    where
      parseFieldType :: ParseFunction (PFieldIdentifier, PTypeExpression)
      parseFieldType Empty = singleError $ RecordStatementEmptyFieldError fieldTypeListRange
      parseFieldType ((TokenSection (IdentifierToken fieldNameRange fieldName)) :<| (TokenSection (ColonToken colonRange)) :<| Empty) =
        singleError $ RecordStatementEmptyFieldTypeError fieldName (fieldNameRange <> colonRange)
      parseFieldType ((TokenSection (IdentifierToken _ fieldName)) :<| (TokenSection (ColonToken _)) :<| typeSections) = do
        fieldType <-
          catchUnboundError (RecordStatementMalformedFieldValueError fieldName (getRange typeSections)) $
            runParserToEnd typeExpressionParser typeSections
        return (fieldName, fieldType)
      parseFieldType sections = singleError $ RecordStatementMalformedFieldError (getRange sections)
parseRecordStatement recTokenSection Empty = singleError $ RecordStatementMalformedError (getRange recTokenSection)
parseRecordStatement recTokenSection restSections = singleError $ RecordStatementMalformedError (getRange recTokenSection <> getRange restSections)

-- Expressions

expressionParser :: Parser PExpression
expressionParser = logicalLevelExpressionParser

-- Logical level

logicalLevelExpressionParser :: Parser PExpression
logicalLevelExpressionParser = do
  leftExpression <- equalityLevelExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toLogicalExpression
    rightExpression <- equalityLevelExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getRange (left, right)) left right

toLogicalExpression :: Section -> Maybe (Range -> PExpression -> PExpression -> PExpression)
toLogicalExpression (TokenSection (AndToken _)) = Just AndExpression
toLogicalExpression (TokenSection (OrToken _)) = Just OrExpression
toLogicalExpression _ = Nothing

-- Equality level

equalityLevelExpressionParser :: Parser PExpression
equalityLevelExpressionParser = do
  leftExpression <- comparisonLevelExpressionParser
  rightSide <- pZeroOrOne $ do
    operator <- pNext <&&> toEqualityExpression
    rightExpression <- comparisonLevelExpressionParser
    return (operator, rightExpression)
  return $ case rightSide of
    Nothing -> leftExpression
    Just (operator, rightExpression) -> operator (getRange (leftExpression, rightExpression)) leftExpression rightExpression

toEqualityExpression :: Section -> Maybe (Range -> PExpression -> PExpression -> PExpression)
toEqualityExpression (TokenSection (EqualEqualToken _)) = Just EqualExpression
toEqualityExpression (TokenSection (NotEqualToken _)) = Just NotEqualExpression
toEqualityExpression _ = Nothing

-- Comparison level

comparisonLevelExpressionParser :: Parser PExpression
comparisonLevelExpressionParser = do
  leftExpression <- additionLevelExpressionParser
  rightSide <- pZeroOrOne $ do
    operator <- pNext <&&> toComparisonExpression
    rightExpression <- additionLevelExpressionParser
    return (operator, rightExpression)
  return $ case rightSide of
    Nothing -> leftExpression
    Just (operator, rightExpression) -> operator (getRange (leftExpression, rightExpression)) leftExpression rightExpression

toComparisonExpression :: Section -> Maybe (Range -> PExpression -> PExpression -> PExpression)
toComparisonExpression (TokenSection (GreaterToken _)) = Just GreaterExpression
toComparisonExpression (TokenSection (LessToken _)) = Just LessExpression
toComparisonExpression (TokenSection (GreaterEqualToken _)) = Just GreaterEqualExpression
toComparisonExpression (TokenSection (LessEqualToken _)) = Just LessEqualExpression
toComparisonExpression _ = Nothing

-- Addition/subtraction level

additionLevelExpressionParser :: Parser PExpression
additionLevelExpressionParser = do
  leftExpression <- multiplicationLevelExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toAddSubtractExpression
    rightExpression <- multiplicationLevelExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getRange (left, right)) left right

toAddSubtractExpression :: Section -> Maybe (Range -> PExpression -> PExpression -> PExpression)
toAddSubtractExpression (TokenSection (PlusToken _)) = Just AddExpression
toAddSubtractExpression (TokenSection (MinusToken _)) = Just SubtractExpression
toAddSubtractExpression _ = Nothing

-- Multiplication/division/modulo level

multiplicationLevelExpressionParser :: Parser PExpression
multiplicationLevelExpressionParser = do
  leftExpression <- prefixExpressionParser
  rightExpressions <- pZeroOrMore $ do
    operator <- pNext <&&> toMultiplyDivideExpression
    rightExpression <- prefixExpressionParser
    return (operator, rightExpression)
  return $ foldl' makeExpression leftExpression rightExpressions
  where
    makeExpression left (operator, right) = operator (getRange (left, right)) left right

toMultiplyDivideExpression :: Section -> Maybe (Range -> PExpression -> PExpression -> PExpression)
toMultiplyDivideExpression (TokenSection (StarToken _)) = Just MultiplyExpression
toMultiplyDivideExpression (TokenSection (SlashToken _)) = Just DivideExpression
toMultiplyDivideExpression (TokenSection (PercentToken _)) = Just ModuloExpression
toMultiplyDivideExpression _ = Nothing

-- Unary prefix level

prefixExpressionParser :: Parser PExpression
prefixExpressionParser = do
  operators <- pZeroOrMore $ pNext <&&> toUnaryExpressionAndRange
  innerExpression <- postfixExpressionParser
  return $ foldr makeExpression innerExpression operators
  where
    makeExpression (operator, range) innerExpression = operator (range <> getRange innerExpression) innerExpression

toUnaryExpressionAndRange :: Section -> Maybe (Range -> PExpression -> PExpression, Range)
toUnaryExpressionAndRange (TokenSection (MinusToken range)) = Just (NegateExpression, range)
toUnaryExpressionAndRange (TokenSection (BangToken range)) = Just (NotExpression, range)
toUnaryExpressionAndRange _ = Nothing

-- Unary postfix level

postfixExpressionParser :: Parser PExpression
postfixExpressionParser = do
  innerExpression <- primaryExpressionParser
  operators <- pZeroOrMore $ postfixOperatorParser
  return $ foldl' (&) innerExpression operators

postfixOperatorParser :: Parser (PExpression -> PExpression)
postfixOperatorParser = fieldAccessParser <|> functionCallParser

fieldAccessParser :: Parser (PExpression -> PExpression)
fieldAccessParser = do
  pNext <&&> matchDotSection
  (fieldRange, fieldName) <- pNext <&&> matchIdentifierSection
  return $ \innerExpression -> FieldAccessExpression (getRange innerExpression <> fieldRange) innerExpression fieldName

matchDotSection :: Section -> Maybe ()
matchDotSection (TokenSection (DotToken _)) = Just ()
matchDotSection _ = Nothing

matchIdentifierSection :: Section -> Maybe (Range, UnboundIdentifier)
matchIdentifierSection (TokenSection (IdentifierToken range identifier)) = Just (range, identifier)
matchIdentifierSection _ = Nothing

functionCallParser :: Parser (PExpression -> PExpression)
functionCallParser = do
  (argumentListRange, argumentListSections) <- pNext <&&> matchSquareBracketSection
  arguments <- returnWithErrors $ parseArguments argumentListRange argumentListSections
  return $ \innerExpression -> FunctionCallExpression (getRange innerExpression <> argumentListRange) innerExpression arguments

parseArguments :: Range -> ParseFunction (Seq PExpression)
parseArguments argumentsRange sections = do
  let argumentSectionLists = seqSplitOn isCommaSection sections
  arguments <- consolidateErrors $ parseArgument <$> argumentSectionLists
  return arguments
  where
    parseArgument :: Seq Section -> WithErrors PExpression
    parseArgument Empty = singleError $ FunctionCallEmptyArgumentError argumentsRange
    parseArgument argumentSections =
      catchUnboundError (FunctionCallMalformedArgumentError $ getRange argumentSections) $
        runParserToEnd expressionParser argumentSections

isCommaSection :: Section -> Bool
isCommaSection (TokenSection (CommaToken _)) = True
isCommaSection _ = False

matchSquareBracketSection :: Section -> Maybe (Range, Seq Section)
matchSquareBracketSection (SquareBracketSection range innerSections) = Just (range, innerSections)
matchSquareBracketSection _ = Nothing

-- Primary level

primaryExpressionParser :: Parser PExpression
primaryExpressionParser =
  parenthesesExpressionParser
    <|> functionExpressionParser
    <|> scopeExpressionParser
    <|> ifExpressionParser
    <|> recordExpressionParser
    <|> literalOrVariableExpressionParser

literalOrVariableExpressionParser :: Parser PExpression
literalOrVariableExpressionParser = pNext <&&> toLiteralExpression

toLiteralExpression :: Section -> Maybe PExpression
toLiteralExpression (TokenSection (IntLiteralToken range value)) = Just $ IntLiteralExpression range value
toLiteralExpression (TokenSection (FloatLiteralToken range value)) = Just $ FloatLiteralExpression range value
toLiteralExpression (TokenSection (CharLiteralToken range value)) = Just $ CharLiteralExpression range value
toLiteralExpression (TokenSection (StringLiteralToken range value)) = Just $ StringLiteralExpression range value
toLiteralExpression (TokenSection (BoolLiteralToken range value)) = Just $ BoolLiteralExpression range value
toLiteralExpression (TokenSection (NilLiteralToken range)) = Just $ NilExpression range
toLiteralExpression (TokenSection (IdentifierToken range identifierName)) = Just $ IdentifierExpression range identifierName
toLiteralExpression _ = Nothing

parenthesesExpressionParser :: Parser PExpression
parenthesesExpressionParser = do
  (range, innerSections) <- pNext <&&> matchParenSection
  returnWithErrors $ catchUnboundError (ExpectedExpressionInParensError range) $ runParserToEnd expressionParser innerSections

matchParenSection :: Section -> Maybe (Range, Seq Section)
matchParenSection (ParenSection range innerSections) = Just (range, innerSections)
matchParenSection _ = Nothing

scopeExpressionParser :: Parser PExpression
scopeExpressionParser = do
  (range, innerSections) <- pNext <&&> matchCurlyBraceSection
  statements <- returnWithErrors $ parseScope innerSections
  return $ ScopeExpression range statements

matchCurlyBraceSection :: Section -> Maybe (Range, Seq Section)
matchCurlyBraceSection (CurlyBraceSection range innerSections) = Just (range, innerSections)
matchCurlyBraceSection _ = Nothing

ifExpressionParser :: Parser PExpression
ifExpressionParser = do
  ifRange <- pNext <&&> matchIfSection
  conditionExpression <- expressionParser
  pNext <&&> matchThenSection
  trueExpression <- expressionParser
  maybeElse <- pZeroOrOne $ pNext <&&> matchElseSection
  case maybeElse of
    Nothing -> do
      let expressionRange = getRange (ifRange, trueExpression)
      return $ IfThenElseExpression expressionRange conditionExpression trueExpression Nothing
    Just _ -> do
      falseExpression <- expressionParser
      let expressionRange = getRange (ifRange, falseExpression)
      return $ IfThenElseExpression expressionRange conditionExpression trueExpression (Just falseExpression)

matchIfSection :: Section -> Maybe Range
matchIfSection (TokenSection (IfToken range)) = Just range
matchIfSection _ = Nothing

matchThenSection :: Section -> Maybe ()
matchThenSection (TokenSection (ThenToken _)) = Just ()
matchThenSection _ = Nothing

matchElseSection :: Section -> Maybe ()
matchElseSection (TokenSection (ElseToken _)) = Just ()
matchElseSection _ = Nothing

functionExpressionParser :: Parser PExpression
functionExpressionParser = do
  (parameterListRange, parameterListSections) <- pNext <&&> matchSquareBracketSection
  returnType <- pZeroOrOne $ do
    pNext <&&> matchColonSection
    typeExpressionParser
  arrowSectionRange <- pNext <&&> matchSingleRightArrowSection
  expressionSections <- pRest
  returnWithErrors $ do
    let expressionErrorRange = case expressionSections of
          Empty -> parameterListRange <> arrowSectionRange
          _ -> getRange expressionSections
    {- We use consolidateErrors2 here so that if parsing the parameter list and function value both return errors, we
       return both errors.
    -}
    (parameterList, body) <-
      consolidateErrors2
        ( parseParamterList parameterListRange parameterListSections,
          catchUnboundError (FunctionMalformedBodyError expressionErrorRange) $
            runParserToEnd expressionParser expressionSections
        )
    let functionExpressionRange = getRange (parameterListRange, expressionErrorRange)
    return $ FunctionExpression functionExpressionRange $ FunctionDefinition functionExpressionRange parameterList (WithTypeAnnotation body returnType)

parseParamterList :: Range -> ParseFunction (Seq (PWithTypeAnnotation PIdentifier))
parseParamterList _ Empty = Success Empty
parseParamterList parameterListRange sections = do
  let parameterSectionLists = seqSplitOn isCommaSection sections
  consolidateErrors $ parseParameter <$> parameterSectionLists
  where
    parseParameter :: Seq Section -> WithErrors (PWithTypeAnnotation PIdentifier)
    parseParameter Empty = singleError $ FunctionEmptyParameterError parameterListRange
    parseParameter (TokenSection (IdentifierToken _ identifierName) :<| Empty) =
      Success $ WithTypeAnnotation identifierName Nothing
    parseParameter
      ( TokenSection (IdentifierToken _ _)
          :<| (TokenSection (ColonToken _))
          :<| Empty
        ) = singleError $ FunctionEmptyParameterTypeError parameterListRange
    parseParameter
      ( TokenSection (IdentifierToken _ identifierName)
          :<| (TokenSection (ColonToken _))
          :<| typeSections
        ) = do
        parameterType <-
          catchUnboundError (FunctionMalformedParameterTypeError $ getRange typeSections) $
            runParserToEnd typeExpressionParser typeSections
        return $ WithTypeAnnotation identifierName (Just parameterType)
    parseParameter _ = singleError $ FunctionMalformedParameterError parameterListRange

matchSingleRightArrowSection :: Section -> Maybe Range
matchSingleRightArrowSection (TokenSection (SingleRightArrowToken range)) = Just range
matchSingleRightArrowSection _ = Nothing

matchColonSection :: Section -> Maybe ()
matchColonSection (TokenSection (ColonToken _)) = Just ()
matchColonSection _ = Nothing

recordExpressionParser :: Parser PExpression
recordExpressionParser = do
  (recordNameRange, recordName) <- pNext <&&> matchIdentifierSection
  (fieldsRange, fieldsSections) <- pNext <&&> matchSquareBracketSection
  -- If there is a set of empty brackets, we want to parse it as a function call, not a record
  pExpect $ isNonEmpty fieldsSections
  fieldsMap <- returnWithErrors $ parseFieldValueList fieldsRange fieldsSections
  return $ RecordExpression (recordNameRange <> fieldsRange) recordName fieldsMap
  where
    isNonEmpty Empty = False
    isNonEmpty _ = True

parseFieldValueList :: Range -> ParseFunction (Map PFieldIdentifier PExpression)
parseFieldValueList _ Empty = Success Map.empty
parseFieldValueList fieldValueListRange sections = do
  let fieldSectionLists = seqSplitOn isCommaSection sections
  fieldValuePairs <- consolidateErrors $ parseFieldValue <$> fieldSectionLists
  foldM addFieldValue Map.empty fieldValuePairs
  where
    parseFieldValue :: ParseFunction (PFieldIdentifier, PExpression)
    parseFieldValue Empty = singleError $ RecordExpressionEmptyFieldError fieldValueListRange
    parseFieldValue (TokenSection (IdentifierToken _ fieldName) :<| TokenSection (EqualsToken _) :<| Empty) =
      singleError $ RecordExpressionEmptyFieldValueError fieldName fieldValueListRange
    parseFieldValue
      ( TokenSection (IdentifierToken _ fieldName)
          :<| TokenSection (EqualsToken _)
          :<| valueSections
        ) = do
        fieldValue <-
          catchUnboundError (RecordExpressionMalformedFieldValueError fieldName (getRange valueSections)) $
            runParserToEnd expressionParser valueSections
        return (fieldName, fieldValue)
    parseFieldValue fieldSections = singleError $ RecordExpressionMalformedFieldError (getRange fieldSections)
    addFieldValue :: Map PFieldIdentifier PExpression -> (PFieldIdentifier, PExpression) -> WithErrors (Map PFieldIdentifier PExpression)
    addFieldValue recordMap (fieldName, fieldValue) = do
      let (maybeConflictingValue, updatedRecordMap) = Map.insertLookupWithKey (\_ a _ -> a) fieldName fieldValue recordMap
      case maybeConflictingValue of
        Just conflictingValue ->
          singleError $ RecordExpressionConflictingFieldsError fieldName (getRange fieldValue) (getRange conflictingValue)
        Nothing -> return updatedRecordMap

-- Types

typeExpressionParser :: Parser PTypeExpression
typeExpressionParser = functionTypeParser <|> primaryTypeParser <|> parenthesesTypeExpressionParser

functionTypeParser :: Parser PTypeExpression
functionTypeParser = do
  (argumentRange, argumentSections) <- pNext <&&> matchSquareBracketSection
  _ <- pNext <&&> matchSingleRightArrowSection
  argumentTypes <- returnWithErrors $ parseParameterTypes argumentRange argumentSections
  returnType <- typeExpressionParser
  return $ FunctionTypeExpression (getRange (argumentRange, returnType)) argumentTypes returnType

parseParameterTypes :: Range -> ParseFunction (Seq PTypeExpression)
parseParameterTypes parameterListRange sections = do
  let parameterSectionLists = seqSplitOn isCommaSection sections
  parameters <- consolidateErrors $ parseParameterType <$> parameterSectionLists
  return parameters
  where
    parseParameterType :: Seq Section -> WithErrors PTypeExpression
    parseParameterType Empty = singleError $ FunctionTypeEmptyParameterError parameterListRange
    parseParameterType parameterSections =
      catchUnboundError (FunctionTypeMalformedParameterError $ getRange parameterSections) $
        runParserToEnd typeExpressionParser parameterSections

primaryTypeParser :: Parser PTypeExpression
primaryTypeParser = pNext <&&> asPrimaryType

asPrimaryType :: Section -> Maybe PTypeExpression
asPrimaryType (TokenSection (IntToken range)) = Just $ IntTypeExpression range
asPrimaryType (TokenSection (FloatToken range)) = Just $ FloatTypeExpression range
asPrimaryType (TokenSection (CharToken range)) = Just $ CharTypeExpression range
asPrimaryType (TokenSection (StringToken range)) = Just $ StringTypeExpression range
asPrimaryType (TokenSection (BoolToken range)) = Just $ BoolTypeExpression range
asPrimaryType (TokenSection (NilToken range)) = Just $ NilTypeExpression range
asPrimaryType (TokenSection (IdentifierToken range identifier)) = Just $ RecordTypeExpression range identifier
asPrimaryType _ = Nothing

parenthesesTypeExpressionParser :: Parser PTypeExpression
parenthesesTypeExpressionParser = do
  (range, innerSections) <- pNext <&&> matchParenSection
  returnWithErrors $ catchUnboundError (ExpectedTypeExpressionInParensError range) $ runParserToEnd typeExpressionParser innerSections
