module Parsing.Parser
  ( parseFile,
  )
where

import Control.Applicative
import Control.Monad (foldM)
import Core.ErrorState
import Core.Errors
import Core.FilePositions
import Core.SyntaxTree
import Core.Utils
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|))
import qualified Data.Sequence as Seq
import Lexing.Tokens
import Parsing.Parsing
import Parsing.SyntaxTree
import Sectioning.Sectioning

parseFile :: ParseFunction PModule
parseFile sections = Module () . MainFunction () <$> parseScope sections

parseScope :: ParseFunction PScope
parseScope sections = do
  let splitSectionLists = seqSplitOn isSemicolon sections
  statementSectionLists <- case splitSectionLists of
    Empty -> return Empty
    statementSectionLists :|> Empty -> return statementSectionLists
    _ :|> finalSections -> singleError $ ExpectedToEndWithSemicolonError $ getRange finalSections
  combinedStatements <- forM' statementSectionLists parseStatement
  let (statements, nonPositionalStatements) = seqPartitionEither combinedStatements
  return $ Scope () nonPositionalStatements statements

isSemicolon :: Section -> Bool
isSemicolon (TokenSection (SemicolonToken _)) = True
isSemicolon _ = False

-- Statements

parseStatement :: ParseFunction (Either PStatement PNonPositionalStatement)
parseStatement Empty = singleError $ ShouldNotGetHereError "To be implemented"
parseStatement (currentSection :<| tailSections) = case currentSection of
  TokenSection (PrintToken _) -> Left <$> parsePrintStatement currentSection tailSections
  TokenSection (LetToken _) -> Left <$> parseVariableDeclarationStatement currentSection tailSections
  TokenSection (MutToken _) -> Left <$> parseMutationStatement currentSection tailSections
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
parseVariableDeclarationStatement letTokenSection sections = execErrorState sections $ do
  mutability <- withUpdateState $ \case
    ((TokenSection (MutToken _)) :<| restSections) -> return (restSections, Mutable)
    restSections -> return (restSections, Immutable)
  variableName <- withUpdateState $ \case
    (TokenSection (IdentifierToken _ identifierName)) :<| restSections -> return (restSections, identifierName)
    _ -> singleError $ VariableDeclarationMalformedError statementRange
  variableType <- withUpdateState $ \case
    (TokenSection (EqualsToken _)) :<| restSections -> return (restSections, Nothing)
    (TokenSection (ColonToken _)) :<| restSections -> do
      case Seq.breakl isEqualsSection restSections of
        (_, Empty) -> singleError $ VariableDeclarationMalformedError statementRange
        (Empty, _) -> singleError $ VariableDeclarationEmptyTypeError statementRange
        (typeSections, _equalsSection :<| valueSections) -> do
          variableType <-
            catchUnboundError (VariableDeclarationMalformedTypeError (getRange typeSections)) $
              runParserToEnd typeExpressionParser typeSections
          return (valueSections, Just variableType)
    _ -> singleError $ VariableDeclarationMalformedError statementRange
  variableValue <-
    getState
      >>= liftWithErrors . \valueSections -> case valueSections of
        Empty -> singleError $ VariableDeclarationEmptyExpressionError statementRange
        _ ->
          catchUnboundError (VariableDeclarationInvalidExpressionError (getRange valueSections)) $
            runParserToEnd expressionParser valueSections
  return $ VariableDeclarationStatement statementRange mutability (WithTypeAnnotation variableName variableType) variableValue
  where
    statementRange = case sections of
      Empty -> getRange letTokenSection
      _ -> getRange (letTokenSection, sections)

isEqualsSection :: Section -> Bool
isEqualsSection (TokenSection (EqualsToken _)) = True
isEqualsSection _ = False

parseMutationStatement :: Section -> ParseFunction PStatement
parseMutationStatement mutTokenSection Empty = singleError $ MutationStatementMalformedError (getRange mutTokenSection)
parseMutationStatement mutTokenSection restSections1 = do
  (leftSections, valueSections) <- case Seq.breakl isEqualsSection restSections1 of
    (_, Empty) -> singleError $ MutationStatementMalformedError statementRange
    (leftSections, _equalsSection :<| valueSections) -> return (leftSections, valueSections)
  makeStatement <- case leftSections of
    (TokenSection (IdentifierToken _ variableName)) :<| Empty -> return $ VariableMutationStatement statementRange variableName
    (recordExpressionSections :|> TokenSection (DotToken _)) :|> TokenSection (IdentifierToken _ fieldName) -> case recordExpressionSections of
      Empty -> singleError $ FieldMutationStatementEmptyRecordError statementRange
      _ -> do
        record <- catchUnboundError (FieldMutationStatementMalformedRecordError (getRange recordExpressionSections)) $ runParserToEnd expressionParser recordExpressionSections
        return $ FieldMutationStatement statementRange record fieldName
    _ -> singleError $ MutationStatementMalformedError (getRange mutTokenSection)
  value <- case valueSections of
    Empty -> singleError $ MutationStatementEmptyValueError statementRange
    _ -> catchUnboundError (MutationStatementMalformedValueError (getRange valueSections)) $ runParserToEnd expressionParser valueSections
  return $ makeStatement value
  where
    statementRange = getRange mutTokenSection <> getRange restSections1

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
parseFunctionStatement funcTokenSection sections = execErrorState sections $ do
  functionName <- withUpdateState $ \case
    ((TokenSection (IdentifierToken _ functionName)) :<| (TokenSection (EqualsToken _)) :<| restSections) -> do
      return (restSections, functionName)
    _ -> singleError $ FunctionStatementMalformedError statementRange
  typeParameters <- withUpdateState $ \case
    ( (AngleBracketSection typeParameterListRange typeParameterListSections)
        :<| (TokenSection (DoubleRightArrowToken _))
        :<| restSections
      ) -> do
        typeParameters <- parseFunctionTypeParameters typeParameterListRange typeParameterListSections
        return (restSections, typeParameters)
    restSections -> return (restSections, Empty)
  parameters <- withUpdateState $ \case
    ( (SquareBracketSection parameterListRange parameterListSections)
        :<| restSections
      ) -> do
        parameters <- parseParameterList parameterListRange parameterListSections
        return (restSections, parameters)
    _ -> singleError $ FunctionStatementMalformedError statementRange
  returnTypeAnnotation <- withUpdateState $ \case
    {- Breaking up the type and body on the leftmost right arrow does mean that if the return type is a function, it may
      need to be put in parentheses to parse successfully. We could try to be smarter about breaking on the correct
      right arrow, but I don't think it's worth it. In these cases, it's probably good for code readability to put the
      type in parentheses anyways.
    -}
    (TokenSection (ColonToken _)) :<| restSections -> case Seq.breakl isSingleRightArrowSection restSections of
      (_, Empty) -> singleError $ FunctionStatementMalformedReturnTypeError statementRange
      (returnTypeSections, _arrow :<| bodySections) -> do
        returnType <- catchUnboundError (FunctionStatementMalformedReturnTypeError (getRange returnTypeSections)) $ runParserToEnd typeExpressionParser returnTypeSections
        return (bodySections, Just returnType)
    (TokenSection (SingleRightArrowToken _) :<| bodySections) -> return (bodySections, Nothing)
    _ -> singleError $ FunctionStatementMalformedError statementRange
  body <-
    getState
      >>= liftWithErrors . \bodySections -> case bodySections of
        Empty -> singleError $ FunctionStatementEmptyBodyError statementRange
        _ -> catchUnboundError (FunctionStatementMalformedBodyError (getRange bodySections)) $ runParserToEnd expressionParser bodySections
  return $ FunctionStatement statementRange functionName typeParameters (FunctionDefinition statementRange parameters (WithTypeAnnotation body returnTypeAnnotation))
  where
    statementRange = case sections of
      Empty -> getRange funcTokenSection
      _ -> getRange (funcTokenSection, sections)

parseFunctionTypeParameters :: Range -> ParseFunction (Seq PTypeParameter)
parseFunctionTypeParameters typeParameterListRange typeParameterListSections = do
  let typeParameterSectionLists = seqSplitOn isCommaSection typeParameterListSections
  consolidateErrors $ parseTypeParameter <$> typeParameterSectionLists
  where
    parseTypeParameter :: ParseFunction PTypeParameter
    parseTypeParameter Empty = singleError $ TypeParameterEmptyError typeParameterListRange
    parseTypeParameter ((TokenSection (IdentifierToken _ typeParameterName)) :<| Empty) = return typeParameterName
    parseTypeParameter typeParameterSections = singleError $ TypeParameterMalformedError (getRange typeParameterSections)

isSingleRightArrowSection :: Section -> Bool
isSingleRightArrowSection (TokenSection (SingleRightArrowToken _)) = True
isSingleRightArrowSection _ = False

parseRecordStatement :: Section -> ParseFunction PNonPositionalStatement
parseRecordStatement recTokenSection sections = execErrorState sections $ do
  recordName <- withUpdateState $ \case
    ( (TokenSection (IdentifierToken _ recordName))
        :<| (TokenSection (EqualsToken _))
        :<| restSections
      ) -> return (restSections, recordName)
    _ -> singleError $ RecordStatementMalformedError statementRange
  (mutabilityParameter, typeParameters) <- withUpdateState $ \case
    ( (AngleBracketSection typeParameterListRange typeParameterListSections)
        :<| (TokenSection (DoubleRightArrowToken _))
        :<| restSections
      ) -> do
        (mutabilityParameter, typeParameters) <- parseRecordTypeParameters typeParameterListRange typeParameterListSections
        return (restSections, (mutabilityParameter, typeParameters))
    restSections -> return (restSections, (Nothing, Empty))
  fieldTypePairs <-
    getState
      >>= liftWithErrors . \case
        ( (SquareBracketSection fieldTypeListRange fieldTypeListSections)
            :<| Empty
          ) -> parseFieldTypes fieldTypeListRange fieldTypeListSections
        _ -> singleError $ RecordStatementMalformedError statementRange
  return $ RecordStatement statementRange recordName mutabilityParameter typeParameters fieldTypePairs
  where
    statementRange = case sections of
      Empty -> getRange recTokenSection
      _ -> getRange (recTokenSection, sections)

parseRecordTypeParameters :: Range -> ParseFunction (Maybe PMutabilityParameter, Seq PTypeParameter)
parseRecordTypeParameters typeParameterListRange typeParameterListSections = do
  let typeAndMutabilityParameterSectionLists = seqSplitOn isCommaSection typeParameterListSections
  (mutabilityParameter, typeParameterSectionLists) <- parseMutabilityParameter typeAndMutabilityParameterSectionLists
  typeParameters <- consolidateErrors $ parseTypeParameter <$> typeParameterSectionLists
  return (mutabilityParameter, typeParameters)
  where
    parseMutabilityParameter :: Seq (Seq Section) -> WithErrors (Maybe PMutabilityParameter, Seq (Seq Section))
    parseMutabilityParameter Empty = return (Nothing, Empty)
    parseMutabilityParameter (firstSectionList :<| restSectionLists) = case firstSectionList of
      ((TokenSection (MutToken _)) :<| (TokenSection (IdentifierToken _ mutabilityParameter)) :<| Empty) -> return (Just mutabilityParameter, restSectionLists)
      ((TokenSection (MutToken mutTokenRange)) :<| Empty) -> singleError $ MutabilityParameterMalformedError mutTokenRange
      ((TokenSection (MutToken mutTokenRange)) :<| restSections) -> singleError $ MutabilityParameterMalformedError (mutTokenRange <> getRange restSections)
      _ -> return (Nothing, firstSectionList :<| restSectionLists)
    parseTypeParameter :: ParseFunction PTypeParameter
    parseTypeParameter Empty = singleError $ TypeParameterEmptyError typeParameterListRange
    parseTypeParameter ((TokenSection (IdentifierToken _ typeParameterName)) :<| Empty) = return typeParameterName
    parseTypeParameter typeParameterSections = singleError $ TypeParameterMalformedError (getRange typeParameterSections)

parseFieldTypes :: Range -> ParseFunction (Seq (PFieldIdentifier, PTypeExpression))
parseFieldTypes fieldTypeListRange fieldTypeListSections = do
  let fieldSectionLists = seqSplitOn isCommaSection fieldTypeListSections
  consolidateErrors $ parseFieldType <$> fieldSectionLists
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
    <|> caseExpressionParser
    <|> recordExpressionParser
    <|> identifierExpressionParser
    <|> literalExpressionParser

literalExpressionParser :: Parser PExpression
literalExpressionParser = pNext <&&> toLiteralExpression

toLiteralExpression :: Section -> Maybe PExpression
toLiteralExpression (TokenSection (IntLiteralToken range value)) = Just $ IntLiteralExpression range value
toLiteralExpression (TokenSection (FloatLiteralToken range value)) = Just $ FloatLiteralExpression range value
toLiteralExpression (TokenSection (CharLiteralToken range value)) = Just $ CharLiteralExpression range value
toLiteralExpression (TokenSection (StringLiteralToken range value)) = Just $ StringLiteralExpression range value
toLiteralExpression (TokenSection (BoolLiteralToken range value)) = Just $ BoolLiteralExpression range value
toLiteralExpression (TokenSection (NilLiteralToken range)) = Just $ NilExpression range
toLiteralExpression _ = Nothing

identifierExpressionParser :: Parser PExpression
identifierExpressionParser = do
  (identifierRange, identifier) <- pNext <&&> matchIdentifierSection
  typeArgumentInfo <- pZeroOrOne $ pNext <&&> matchAngleBracketSection
  case typeArgumentInfo of
    Nothing -> return $ IdentifierExpression identifierRange $ PIdentifier identifier Empty
    Just (typeArgumentsRange, typeArgumentsSections) -> do
      typeArguments <- returnWithErrors $ parseTypeArgumentList typeArgumentsRange typeArgumentsSections
      return $ IdentifierExpression identifierRange $ PIdentifier identifier typeArguments

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
        ( parseParameterList parameterListRange parameterListSections,
          catchUnboundError (FunctionMalformedBodyError expressionErrorRange) $
            runParserToEnd expressionParser expressionSections
        )
    let functionExpressionRange = getRange (parameterListRange, expressionErrorRange)
    return $ FunctionExpression functionExpressionRange $ FunctionDefinition functionExpressionRange parameterList (WithTypeAnnotation body returnType)

parseParameterList :: Range -> ParseFunction (Seq (PWithTypeAnnotation PValueIdentifier))
parseParameterList _ Empty = Success Empty
parseParameterList parameterListRange sections = do
  let parameterSectionLists = seqSplitOn isCommaSection sections
  consolidateErrors $ parseParameter <$> parameterSectionLists
  where
    parseParameter :: Seq Section -> WithErrors (PWithTypeAnnotation PValueIdentifier)
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
  maybeMutRange <- pZeroOrOne $ pNext <&&> matchMutSection
  (recordNameRange, recordName) <- pNext <&&> matchIdentifierSection
  maybeTypeArgumentInfo <- pZeroOrOne $ pNext <&&> matchAngleBracketSection
  typeArguments <- case maybeTypeArgumentInfo of
    Nothing -> return Empty
    Just (typeArgumentsRange, typeArgumentsSections) -> returnWithErrors $ parseTypeArgumentList typeArgumentsRange typeArgumentsSections
  (fieldsRange, fieldsSections) <- pNext <&&> matchSquareBracketSection
  -- If there is a set of empty brackets, we want to parse it as a function call, not a record
  pExpect $ isNonEmpty fieldsSections
  fieldsMap <- returnWithErrors $ parseFieldValueList fieldsRange fieldsSections
  return $ case maybeMutRange of
    Nothing -> RecordExpression (recordNameRange <> fieldsRange) Immutable recordName typeArguments fieldsMap
    Just mutRange -> RecordExpression (mutRange <> fieldsRange) Mutable recordName typeArguments fieldsMap
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
      let (maybeConflictingValue, updatedRecordMap) = insertAndReplace fieldName fieldValue recordMap
      case maybeConflictingValue of
        Just conflictingValue ->
          singleError $ RecordExpressionConflictingFieldsError fieldName (getRange fieldValue) (getRange conflictingValue)
        Nothing -> return updatedRecordMap

parseTypeArgumentList :: Range -> ParseFunction (Seq PTypeExpression)
parseTypeArgumentList typeArgumentListRange Empty = singleError $ TypeArgumentListEmptyError typeArgumentListRange
parseTypeArgumentList typeArgumentListRange typeArgumentListSections = do
  let typeArgumentSectionLists = seqSplitOn isCommaSection typeArgumentListSections
  let parseTypeParameter sections = case sections of
        Empty -> singleError $ TypeArgumentEmptyError typeArgumentListRange
        _ -> catchUnboundError (TypeArgumentMalformedError $ getRange sections) $ runParserToEnd typeExpressionParser sections
  consolidateErrors $ parseTypeParameter <$> typeArgumentSectionLists

matchMutSection :: Section -> Maybe Range
matchMutSection (TokenSection (MutToken range)) = Just range
matchMutSection _ = Nothing

matchAngleBracketSection :: Section -> Maybe (Range, Seq Section)
matchAngleBracketSection (AngleBracketSection range innerSections) = Just (range, innerSections)
matchAngleBracketSection _ = Nothing

caseExpressionParser :: Parser PExpression
caseExpressionParser = do
  caseRange <- pNext <&&> matchCaseSection
  switch <- expressionParser
  pNext <&&> matchOfSection
  (caseListRange, caseListSections) <- pNext <&&> matchSquareBracketSection
  caseList <- returnWithErrors $ parseCaseList caseListRange caseListSections
  return $ CaseExpression (caseRange <> caseListRange) switch caseList

parseCaseList :: Range -> ParseFunction (Map PRecordIdentifier (PValueIdentifier, PExpression))
parseCaseList _ Empty = Success Map.empty
parseCaseList caseListRange sections = do
  let caseSectionLists = seqSplitOn isCommaSection sections
  recordValuePairs <- consolidateErrors $ parseCase <$> caseSectionLists
  foldM addCase Map.empty recordValuePairs
  where
    parseCase :: ParseFunction (PRecordIdentifier, PValueIdentifier, PExpression)
    parseCase Empty = singleError $ CaseExpressionEmptyCaseError caseListRange
    parseCase (TokenSection (IdentifierToken _ recordName) :<| TokenSection (SingleRightArrowToken _) :<| Empty) =
      singleError $ CaseExpressionEmptyCaseValueError recordName caseListRange
    parseCase
      ( TokenSection (IdentifierToken _ recordName)
          :<| TokenSection (ColonToken _)
          :<| TokenSection (IdentifierToken _ switchValueName)
          :<| TokenSection (SingleRightArrowToken _)
          :<| valueSections
        ) = do
        caseValue <-
          catchUnboundError (CaseExpressionMalformedCaseValueError recordName (getRange valueSections)) $
            runParserToEnd expressionParser valueSections
        return (recordName, switchValueName, caseValue)
    parseCase fieldSections = singleError $ CaseExpressionMalformedCaseError (getRange fieldSections)
    addCase ::
      Map PRecordIdentifier (PValueIdentifier, PExpression) ->
      (PRecordIdentifier, PValueIdentifier, PExpression) ->
      WithErrors (Map PRecordIdentifier (PValueIdentifier, PExpression))
    addCase caseMap (recordName, switchValueName, caseValue) = do
      let (maybeConflictingValue, updatedCaseMap) = Map.insertLookupWithKey (\_ a _ -> a) recordName (switchValueName, caseValue) caseMap
      case maybeConflictingValue of
        Just conflictingValue ->
          singleError $ CaseExpressionDuplicatedCasesError recordName (getRange caseValue) (getRange . snd $ conflictingValue)
        Nothing -> return updatedCaseMap

matchCaseSection :: Section -> Maybe Range
matchCaseSection (TokenSection (CaseToken range)) = Just range
matchCaseSection _ = Nothing

matchOfSection :: Section -> Maybe ()
matchOfSection (TokenSection (OfToken _)) = Just ()
matchOfSection _ = Nothing

-- Types

typeExpressionParser :: Parser PTypeExpression
typeExpressionParser = functionTypeParser <|> identifierOrRecordUnionTypeParser <|> simpleTypeParser <|> parenthesesTypeExpressionParser

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

identifierOrRecordUnionTypeParser :: Parser PTypeExpression
identifierOrRecordUnionTypeParser = do
  (mutability, firstRecordName, startRange) <- mutabilityParameterParser <|> mutableParser <|> immutableParser
  (firstRecordTypeArguments, maybeFirstRecordTypeArgumentsRange) <- typeArgumentParser
  let firstRecordRange = case maybeFirstRecordTypeArgumentsRange of
        Nothing -> startRange
        Just firstRecordTypeArgumentsRange -> startRange <> firstRecordTypeArgumentsRange
  restRecords <- pZeroOrMore $ do
    _ <- pNext <&&> matchPipeSection
    (recordNameRange, recordName) <- pNext <&&> matchIdentifierSection
    (recordTypeArguments, maybeTypeArgumentsRange) <- typeArgumentParser
    let recordRange = case maybeTypeArgumentsRange of
          Nothing -> recordNameRange
          Just typeArgumentsRange -> typeArgumentsRange
    return ((recordName, recordTypeArguments), recordRange)
  case (mutability, restRecords, firstRecordTypeArguments) of
    {- This case is when we have a single identifier with no type arguments. It might be a record or it might be a type
      parameter. This case is handled here rather than in simpleTypeParser because there would be issues telling a
      mutability parameter from a standalone identifier type expression.
    -}
    (Left Immutable, Empty, Empty) -> return $ IdentifierTypeExpression firstRecordRange firstRecordName
    _ -> do
      let recordsWithRange = ((firstRecordName, firstRecordTypeArguments), firstRecordRange) <| restRecords
      return $ RecordUnionTypeExpression (snd (seqHead recordsWithRange) <> snd (seqTail recordsWithRange)) mutability (fst <$> recordsWithRange)
  where
    mutableParser = do
      mutSectionRange <- pNext <&&> matchMutSection
      (firstRecordNameRange, firstRecordName) <- pNext <&&> matchIdentifierSection
      return (Left Mutable, firstRecordName, mutSectionRange <> firstRecordNameRange)
    immutableParser = do
      (firstRecordNameRange, firstRecordName) <- pNext <&&> matchIdentifierSection
      return (Left Immutable, firstRecordName, firstRecordNameRange)
    mutabilityParameterParser = do
      (mutabilityParameterRange, mutabilityParameterName) <- pNext <&&> matchIdentifierSection
      (firstRecordNameRange, firstRecordName) <- pNext <&&> matchIdentifierSection
      return (Right mutabilityParameterName, firstRecordName, mutabilityParameterRange <> firstRecordNameRange)
    typeArgumentParser = do
      typeArgumentListInfo <- pZeroOrOne $ pNext <&&> matchAngleBracketSection
      case typeArgumentListInfo of
        Nothing -> return (Empty, Nothing)
        Just (typeArgumentsRange, typeArgumentsSections) -> do
          typeArguments <- returnWithErrors $ parseTypeArgumentList typeArgumentsRange typeArgumentsSections
          return (typeArguments, Just typeArgumentsRange)

matchPipeSection :: Section -> Maybe Range
matchPipeSection (TokenSection (PipeToken range)) = Just range
matchPipeSection _ = Nothing

simpleTypeParser :: Parser PTypeExpression
simpleTypeParser = pNext <&&> asSimpleType

asSimpleType :: Section -> Maybe PTypeExpression
asSimpleType (TokenSection (IntToken range)) = Just $ IntTypeExpression range
asSimpleType (TokenSection (FloatToken range)) = Just $ FloatTypeExpression range
asSimpleType (TokenSection (CharToken range)) = Just $ CharTypeExpression range
asSimpleType (TokenSection (StringToken range)) = Just $ StringTypeExpression range
asSimpleType (TokenSection (BoolToken range)) = Just $ BoolTypeExpression range
asSimpleType (TokenSection (NilToken range)) = Just $ NilTypeExpression range
asSimpleType _ = Nothing

parenthesesTypeExpressionParser :: Parser PTypeExpression
parenthesesTypeExpressionParser = do
  (range, innerSections) <- pNext <&&> matchParenSection
  returnWithErrors $ catchUnboundError (ExpectedTypeExpressionInParensError range) $ runParserToEnd typeExpressionParser innerSections
