module Core.Errors
  ( Error
      ( ShouldNotGetHereError,
        LexError,
        UnterminatedStringError,
        UnterminatedCharError,
        InvalidCharLiteralError,
        UnmatchedStartGroupingError,
        UnmatchEndGroupingError,
        MismatchedGroupingEndsError,
        ExpectedExpressionInParensError,
        ExpectedToEndWithSemicolonError,
        PrintStatementEmptyExpressionError,
        PrintStatementInvalidExpressionError,
        VariableDeclarationMalformedError,
        VariableDeclarationEmptyExpressionError,
        VariableDeclarationInvalidExpressionError,
        VariableMutationMalformedError,
        VariableMutationEmptyExpressionError,
        VariableMutationInvalidExpressionError,
        ExpressionStatementInvalidExpressionError,
        WhileStatementNoLoopError,
        WhileStatementEmptyConditionError,
        WhileStatementEmptyStatementError,
        WhileStatementMalformedConditionExpressionError,
        WhileStatementMalformedBodyExpressionError,
        FunctionCallEmptyArgumentError,
        FunctionCallMalformedArgumentError,
        FunctionMalformedParameterListError,
        FunctionMalformedBodyError,
        ReturnStatementInvalidExpressionError,
        ConflictingVariableDeclarationsError,
        VariableUndefinedAtReferenceError,
        VariableDeclaredAfterReferenceError,
        VariableReferencedInDeclarationError,
        VariableShadowedInDeclarationError,
        ConflictingParameterNamesError,
        MutatedImmutableVariableError,
        MutatedParameterError,
        MutatedCapturedIdentifierError,
        RuntimeError
      ),
    WithErrors (Error, Success),
    singleError,
    foldrWithErrors,
    consolidateErrors,
    consolidateErrors2,
  )
where

import Core.FilePositions
import Core.Utils
import Data.Sequence (Seq (Empty), singleton, (<|), (><))
import Data.Text (Text)
import qualified Data.Text as Text
import Lexing.Tokens

data Error
  = -- Generic
    ShouldNotGetHereError String
  | -- Lexing
    LexError Position
  | UnterminatedStringError Position
  | UnterminatedCharError Position
  | InvalidCharLiteralError Position
  | -- Sectioning
    UnmatchedStartGroupingError Token
  | UnmatchEndGroupingError Token
  | MismatchedGroupingEndsError Token Token
  | -- Parsing
    ExpectedExpressionInParensError Range
  | ExpectedToEndWithSemicolonError Range
  | PrintStatementEmptyExpressionError Range
  | PrintStatementInvalidExpressionError Range
  | VariableDeclarationMalformedError Range
  | VariableDeclarationEmptyExpressionError Range
  | VariableDeclarationInvalidExpressionError Range
  | VariableMutationMalformedError Range
  | VariableMutationEmptyExpressionError Range
  | VariableMutationInvalidExpressionError Range
  | ExpressionStatementInvalidExpressionError Range
  | WhileStatementNoLoopError Range
  | WhileStatementEmptyConditionError Range
  | WhileStatementEmptyStatementError Range
  | WhileStatementMalformedConditionExpressionError Range
  | WhileStatementMalformedBodyExpressionError Range
  | FunctionCallEmptyArgumentError Range
  | FunctionCallMalformedArgumentError Range
  | FunctionMalformedParameterListError Range
  | FunctionMalformedBodyError Range
  | ReturnStatementInvalidExpressionError Range
  | -- Variable binding
    ConflictingVariableDeclarationsError Text Range Range
  | VariableUndefinedAtReferenceError Text Range
  | VariableDeclaredAfterReferenceError Text Range Range
  | VariableReferencedInDeclarationError Text Range Range
  | VariableShadowedInDeclarationError Text Range Range
  | ConflictingParameterNamesError Text Range Range
  | MutatedImmutableVariableError Text Range Range
  | MutatedParameterError Text Range Range
  | MutatedCapturedIdentifierError Text Range Range
  | -- Runtime
    RuntimeError Int String
  deriving (Show, Eq)

instance Pretty Error where
  pretty (ShouldNotGetHereError message) = "Should not get here: " ++ message
  pretty (LexError position) = "Encountered unexpected character at " ++ pretty position
  pretty (UnterminatedStringError position) = "Encountered unteriminated string at " ++ pretty position
  pretty (UnterminatedCharError position) = "Encountered unteriminated char at " ++ pretty position
  pretty (InvalidCharLiteralError position) = "Char literals must contain exactly one character " ++ pretty position
  pretty (UnmatchedStartGroupingError token) = "Encountered unmatched token " ++ pretty token ++ " at " ++ pretty (getRange token)
  pretty (UnmatchEndGroupingError token) = "Encountered unmatched token " ++ pretty token ++ " at " ++ pretty (getRange token)
  pretty (MismatchedGroupingEndsError startToken endToken) =
    "Encountered mismatched grouping ends "
      ++ pretty startToken
      ++ " and "
      ++ pretty endToken
      ++ " at "
      ++ pretty (getRange (startToken, endToken))
  pretty (ExpectedExpressionInParensError range) = "Failed to parse contents of parentheses as an expression at" ++ pretty range
  pretty (ExpectedToEndWithSemicolonError range) = "Statement must end with a semicolon " ++ pretty range
  pretty (PrintStatementEmptyExpressionError range) = "Print statement must have an expression at " ++ pretty range
  pretty (PrintStatementInvalidExpressionError range) = "Failed to parse argument of print statement as an expression at " ++ pretty range
  pretty (VariableDeclarationMalformedError range) = "Failed to parse variable declaration statement at " ++ pretty range
  pretty (VariableDeclarationEmptyExpressionError range) = "Variable declaration must have an expression at " ++ pretty range
  pretty (VariableDeclarationInvalidExpressionError range) = "Failed to parse variable declaration value as an expression at " ++ pretty range
  pretty (VariableMutationMalformedError range) = "Failed to parse variable mutation statement at " ++ pretty range
  pretty (VariableMutationEmptyExpressionError range) = "Variable mutation must have an expression at " ++ pretty range
  pretty (VariableMutationInvalidExpressionError range) = "Failed to parse variable mutation value as an expression at " ++ pretty range
  pretty (ExpressionStatementInvalidExpressionError range) = "Failed to parse statement as an expression at " ++ pretty range
  pretty (WhileStatementNoLoopError range) = "While loop statement has no loop keyword at" ++ pretty range
  pretty (WhileStatementEmptyConditionError range) = "While loop statement has an empty condition at " ++ pretty range
  pretty (WhileStatementEmptyStatementError range) = "While loop statement has an empty statement at " ++ pretty range
  pretty (WhileStatementMalformedConditionExpressionError range) = "Failed to parse condition of while loop statement as an expression at " ++ pretty range
  pretty (WhileStatementMalformedBodyExpressionError range) = "Failed to parse body of while loop statement as an expression at " ++ pretty range
  pretty (FunctionCallEmptyArgumentError range) = "Found empty argument in function call at " ++ pretty range
  pretty (FunctionCallMalformedArgumentError range) = "Failed to parse function argument as an expression at " ++ pretty range
  pretty (FunctionMalformedParameterListError range) = "Failed to parse parameter list of function at " ++ pretty range
  pretty (FunctionMalformedBodyError range) = "Failed to parse function body as an expression at " ++ pretty range
  pretty (ReturnStatementInvalidExpressionError range) = "Failed to parse argument of return statement as an expression at " ++ pretty range
  pretty (ConflictingVariableDeclarationsError variableName declarationRange1 declarationRange2) =
    "Variable " ++ Text.unpack variableName ++ " has conflicting declarations at " ++ pretty declarationRange1 ++ " and " ++ pretty declarationRange2
  pretty (VariableUndefinedAtReferenceError variableName range) =
    "Variable " ++ Text.unpack variableName ++ " is not defined before it is referenced at " ++ pretty range
  pretty (VariableDeclaredAfterReferenceError variableName referenceRange delcarationRange) =
    "Variable " ++ Text.unpack variableName ++ " is defined at " ++ pretty delcarationRange ++ " before it is referenced at " ++ pretty referenceRange
  pretty (VariableReferencedInDeclarationError variableName declarationRange usageRange) =
    "Variable " ++ Text.unpack variableName ++ " is referenced at " ++ pretty usageRange ++ " inside its declaration at " ++ pretty declarationRange
  pretty (VariableShadowedInDeclarationError variableName declarationRange shadowingRange) =
    "Variable " ++ Text.unpack variableName ++ " is shadowed at " ++ pretty shadowingRange ++ " inside its declaration at " ++ pretty declarationRange
  pretty (ConflictingParameterNamesError variableName parameterRange1 parameterRange2) =
    Text.unpack variableName ++ " is used as a parameter twice in the same function definition at " ++ pretty parameterRange1 ++ " and " ++ pretty parameterRange2
  pretty (MutatedImmutableVariableError variableName declarationRange mutationRange) =
    "Variable " ++ Text.unpack variableName ++ " declared as immutable at " ++ pretty declarationRange ++ " is mutated at " ++ pretty mutationRange
  pretty (MutatedParameterError parameterName declarationRange mutationRange) =
    "Function parameter " ++ Text.unpack parameterName ++ " defined at " ++ pretty declarationRange ++ " is mutated at " ++ pretty mutationRange
  pretty (MutatedCapturedIdentifierError identifierName declarationRange mutationRange) =
    "Identifier " ++ Text.unpack identifierName ++ " defined at " ++ pretty declarationRange ++ " cannot be mutated at " ++ pretty mutationRange ++ " in a nested function"
  pretty (RuntimeError exitCode stdErr) = "VM failed with exit code " ++ show exitCode ++ " and stdErr " ++ stdErr

data WithErrors a
  = Error (Seq Error)
  | Success a
  deriving (Eq, Show)

singleError :: Error -> WithErrors a
singleError = Error . singleton

instance Functor WithErrors where
  fmap f (Success a) = Success $ f a
  fmap _ (Error es) = Error es

instance Applicative WithErrors where
  pure = Success
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e
  (<*>) (Success f) (Success x) = Success (f x)

instance Monad WithErrors where
  (>>=) m f = case m of
    Error e -> Error e
    Success r -> f r

{- An alternate version of the Applicative operator <*> that if both argument error, passes through both sets of errors.
Unfortunately, this can't be the actual Applicative implementation, as there is no compatible Monad instance.
-}
(<***>) :: WithErrors (a -> b) -> WithErrors a -> WithErrors b
weF <***> weA = do
  (f, a) <- consolidateErrors2 (weF, weA)
  return $ f a

liftA2' :: (a -> b -> c) -> (WithErrors a -> WithErrors b -> WithErrors c)
liftA2' f a b = (f <$> a) <***> b

foldrWithErrors :: (Foldable t) => (a -> b -> b) -> b -> t (WithErrors a) -> WithErrors b
foldrWithErrors combine seed = foldr (liftA2' combine) (Success seed)

consolidateErrors :: Seq (WithErrors a) -> WithErrors (Seq a)
consolidateErrors = foldrWithErrors (<|) Empty

consolidateErrors2 :: (WithErrors a, WithErrors b) -> WithErrors (a, b)
consolidateErrors2 (Success a, Success b) = Success (a, b)
consolidateErrors2 (Error es, Success _) = Error es
consolidateErrors2 (Success _, Error es) = Error es
consolidateErrors2 (Error es1, Error es2) = Error $ es1 >< es2