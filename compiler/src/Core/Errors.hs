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
        VariableDeclarationEmptyTypeError,
        VariableDeclarationMalformedTypeError,
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
        FunctionEmptyParameterError,
        FunctionMalformedParameterError,
        FunctionEmptyParameterTypeError,
        FunctionMalformedParameterTypeError,
        FunctionMalformedBodyError,
        ReturnStatementInvalidExpressionError,
        FunctionTypeEmptyParameterError,
        FunctionTypeMalformedParameterError,
        ExpectedTypeExpressionInParensError,
        ConflictingVariableDeclarationsError,
        VariableUndefinedAtReferenceError,
        VariableDeclaredAfterReferenceError,
        VariableReferencedInDeclarationError,
        VariableShadowedInDeclarationError,
        ConflictingParameterNamesError,
        MutatedImmutableVariableError,
        MutatedParameterError,
        MutatedCapturedIdentifierError,
        VariableDeclarationTypeError,
        VariableMutationTypeError,
        WhileLoopConditionTypeError,
        MainFunctionReturnTypeError,
        FunctionReturnTypeError,
        NegateExpressionTypeError,
        AddExpressionTypeError,
        SubtractExpressionTypeError,
        MultiplyExpressionTypeError,
        DivideExpressionTypeError,
        ModuloExpressionTypeError,
        NotExpressionTypeError,
        AndExpressionTypeError,
        OrExpressionTypeError,
        EqualExpressionTypeError,
        NotEqualExpressionTypeError,
        GreaterExpressionTypeError,
        LessExpressionTypeError,
        GreaterEqualExpressionTypeError,
        LessEqualExpressionTypeError,
        IfThenElseExpressionConditionTypeError,
        IfThenExpressionBranchesTypeError,
        IfThenElseExpressionBranchesTypeError,
        FunctionCallExpressionNotAFunctionTypeError,
        FunctionCallExpressionArityError,
        FunctionCallExpressionArgumentTypeError,
        FunctionMissingParameterTypeAnnotation,
        FunctionMissingReturnTypeAnnotation,
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
import Core.Type
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
  | VariableDeclarationEmptyTypeError Range
  | VariableDeclarationMalformedTypeError Range
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
  | FunctionEmptyParameterError Range
  | FunctionMalformedParameterError Range
  | FunctionEmptyParameterTypeError Range
  | FunctionMalformedParameterTypeError Range
  | FunctionMalformedBodyError Range
  | ReturnStatementInvalidExpressionError Range
  | FunctionTypeEmptyParameterError Range
  | FunctionTypeMalformedParameterError Range
  | ExpectedTypeExpressionInParensError Range
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
  | -- Type checking
    VariableDeclarationTypeError Range Type Type
  | VariableMutationTypeError Range Type Type
  | WhileLoopConditionTypeError Range Type
  | MainFunctionReturnTypeError Range Type
  | FunctionReturnTypeError Range Type Range Type
  | NegateExpressionTypeError Range Type
  | AddExpressionTypeError Range Type Type
  | SubtractExpressionTypeError Range Type Type
  | MultiplyExpressionTypeError Range Type Type
  | DivideExpressionTypeError Range Type Type
  | ModuloExpressionTypeError Range Type Type
  | NotExpressionTypeError Range Type
  | AndExpressionTypeError Range Type Type
  | OrExpressionTypeError Range Type Type
  | EqualExpressionTypeError Range Type Type
  | NotEqualExpressionTypeError Range Type Type
  | GreaterExpressionTypeError Range Type Type
  | LessExpressionTypeError Range Type Type
  | GreaterEqualExpressionTypeError Range Type Type
  | LessEqualExpressionTypeError Range Type Type
  | IfThenElseExpressionConditionTypeError Range Type
  | IfThenExpressionBranchesTypeError Range Type
  | IfThenElseExpressionBranchesTypeError Range Type Type
  | FunctionCallExpressionNotAFunctionTypeError Range Type
  | FunctionCallExpressionArityError Range Int Int
  | FunctionCallExpressionArgumentTypeError Range Type Type
  | FunctionMissingParameterTypeAnnotation Range
  | FunctionMissingReturnTypeAnnotation Range
  | -- Runtime
    RuntimeError Int String
  deriving (Eq)

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
  pretty (VariableDeclarationEmptyTypeError range) = "Variable type annotation must have a type at " ++ pretty range
  pretty (VariableDeclarationMalformedTypeError range) = "Failed to parse variable type annotation as a type at " ++ pretty range
  pretty (ExpressionStatementInvalidExpressionError range) = "Failed to parse statement as an expression at " ++ pretty range
  pretty (WhileStatementNoLoopError range) = "While loop statement has no loop keyword at" ++ pretty range
  pretty (WhileStatementEmptyConditionError range) = "While loop statement has an empty condition at " ++ pretty range
  pretty (WhileStatementEmptyStatementError range) = "While loop statement has an empty statement at " ++ pretty range
  pretty (WhileStatementMalformedConditionExpressionError range) = "Failed to parse condition of while loop statement as an expression at " ++ pretty range
  pretty (WhileStatementMalformedBodyExpressionError range) = "Failed to parse body of while loop statement as an expression at " ++ pretty range
  pretty (FunctionCallEmptyArgumentError range) = "Found empty argument in function call at " ++ pretty range
  pretty (FunctionCallMalformedArgumentError range) = "Failed to parse function argument as an expression at " ++ pretty range
  pretty (FunctionEmptyParameterError range) = "Function had an empty parameter at " ++ pretty range
  pretty (FunctionMalformedParameterError range) = "Failed to parse function parameter at " ++ pretty range
  pretty (FunctionEmptyParameterTypeError range) = "Function parameter type annotation was empty at " ++ pretty range
  pretty (FunctionMalformedParameterTypeError range) = "Failed to parse function parameter type annotation as a type at " ++ pretty range
  pretty (FunctionMalformedBodyError range) = "Failed to parse function body as an expression at " ++ pretty range
  pretty (ReturnStatementInvalidExpressionError range) = "Failed to parse argument of return statement as an expression at " ++ pretty range
  pretty (FunctionTypeEmptyParameterError range) = "Function type had an empty parameter at " ++ pretty range
  pretty (FunctionTypeMalformedParameterError range) = "Failed to parse parameter of function type as a type at " ++ pretty range
  pretty (ExpectedTypeExpressionInParensError range) = "Failed to parse contents of parentheses as a type at " ++ pretty range
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
  pretty (VariableDeclarationTypeError range expectedType actualType) =
    "In variable declaration at " ++ pretty range ++ ", the variable has type " ++ pretty expectedType ++ ", but the value has type " ++ pretty actualType
  pretty (VariableMutationTypeError range expectedType actualType) =
    "In variable mutation at " ++ pretty range ++ ", the variable has type " ++ pretty expectedType ++ ", but the value has type " ++ pretty actualType
  pretty (WhileLoopConditionTypeError range conditionType) =
    "While loop condition should have type Bool, but is " ++ pretty conditionType ++ " at " ++ pretty range
  pretty (MainFunctionReturnTypeError range returnType) =
    "Return from main function should have type Nil, but is " ++ pretty returnType ++ " at " ++ pretty range
  pretty (FunctionReturnTypeError expectedTypeRange expectedType returnStatementRange actualType) =
    "Return from function should have type " ++ pretty expectedType ++ " set at " ++ pretty expectedTypeRange ++ ", but is " ++ pretty actualType ++ " at " ++ pretty returnStatementRange
  pretty (NegateExpressionTypeError range innerType) = "Cannot negate expression of type " ++ pretty innerType ++ " at " ++ pretty range
  pretty (AddExpressionTypeError range leftType rightType) =
    "Cannot add expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (SubtractExpressionTypeError range leftType rightType) =
    "Cannot subtract expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (MultiplyExpressionTypeError range leftType rightType) =
    "Cannot multiply expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (DivideExpressionTypeError range leftType rightType) =
    "Cannot divide expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (ModuloExpressionTypeError range leftType rightType) =
    "Cannot modulo expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (NotExpressionTypeError range innerType) = "Cannot not expression of type " ++ pretty innerType ++ " at " ++ pretty range
  pretty (AndExpressionTypeError range leftType rightType) =
    "Cannot and expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (OrExpressionTypeError range leftType rightType) =
    "Cannot or expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (EqualExpressionTypeError range leftType rightType) =
    "Cannot check equality of expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (NotEqualExpressionTypeError range leftType rightType) =
    "Cannot check inequality of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (GreaterExpressionTypeError range leftType rightType) =
    "Cannot compare expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (LessExpressionTypeError range leftType rightType) =
    "Cannot compare expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (GreaterEqualExpressionTypeError range leftType rightType) =
    "Cannot compare expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (LessEqualExpressionTypeError range leftType rightType) =
    "Cannot compare expressions of type " ++ pretty leftType ++ " and " ++ pretty rightType ++ " at " ++ pretty range
  pretty (IfThenElseExpressionConditionTypeError range conditionType) =
    "If expression condition should have type Bool, but is " ++ pretty conditionType ++ " at " ++ pretty range
  pretty (IfThenExpressionBranchesTypeError range trueBranchType) =
    "If expression true branch should have type Nil when there is no false branch, but has type" ++ pretty trueBranchType ++ " at " ++ pretty range
  pretty (IfThenElseExpressionBranchesTypeError range trueBranchType falseBranchType) =
    "Branches of if expression have differring types " ++ pretty trueBranchType ++ " and " ++ pretty falseBranchType ++ " at " ++ pretty range
  pretty (FunctionCallExpressionNotAFunctionTypeError range functionType) =
    "Attempted to call a value of non-function type " ++ pretty functionType ++ " at " ++ pretty range
  pretty (FunctionCallExpressionArityError range numParameters numArguments) =
    "Attempted to call function with " ++ show numParameters ++ " parameters with " ++ show numArguments ++ " at " ++ pretty range
  pretty (FunctionCallExpressionArgumentTypeError range parameterType argumentType) =
    "When calling functtion, parameter type " ++ pretty parameterType ++ " does not match argument type " ++ pretty argumentType ++ " at " ++ pretty range
  pretty (FunctionMissingParameterTypeAnnotation range) =
    "Function parameter has no type annotation at " ++ pretty range
  pretty (FunctionMissingReturnTypeAnnotation range) =
    "Function has no return type annotation at " ++ pretty range
  pretty (RuntimeError exitCode stdErr) = "VM failed with exit code " ++ show exitCode ++ " and stdErr " ++ stdErr

instance Show Error where
  show = pretty

data WithErrors a
  = Error (Seq Error)
  | Success a
  deriving (Eq)

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