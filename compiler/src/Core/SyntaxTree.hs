{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.SyntaxTree
  ( Module (Module),
    ModuleData,
    MainFunction (MainFunction),
    MainFunctionData,
    Scope (Scope),
    ScopeData,
    getScopeData,
    FunctionDefinition (FunctionDefinition),
    FunctionDefinitionData,
    getFunctionDefinitionData,
    Statement
      ( PrintStatement,
        VariableDeclarationStatement,
        VariableMutationStatement,
        FieldMutationStatement,
        ExpressionStatement,
        WhileLoopStatement,
        ReturnStatement
      ),
    StatementData,
    getStatementData,
    NonPositionalStatement
      ( FunctionStatement,
        RecordStatement
      ),
    NonPositionalStatementData,
    Mutability (Mutable, Immutable),
    Identifier,
    ValueIdentifier,
    FunctionIdentifier,
    RecordIdentifier,
    FieldIdentifier,
    TypeIdentifier,
    TypeParameter,
    MutabilityParameter,
    Expression
      ( IntLiteralExpression,
        FloatLiteralExpression,
        CharLiteralExpression,
        StringLiteralExpression,
        BoolLiteralExpression,
        NilExpression,
        NegateExpression,
        AddExpression,
        SubtractExpression,
        MultiplyExpression,
        DivideExpression,
        ModuloExpression,
        NotExpression,
        AndExpression,
        OrExpression,
        EqualExpression,
        NotEqualExpression,
        GreaterExpression,
        LessExpression,
        GreaterEqualExpression,
        LessEqualExpression,
        IdentifierExpression,
        IfThenElseExpression,
        ScopeExpression,
        FunctionExpression,
        FunctionCallExpression,
        RecordExpression,
        FieldAccessExpression,
        CaseExpression
      ),
    ExpressionData,
    getExpressionData,
    RecordFieldValues,
    TypeArguments,
    WithTypeAnnotation (WithTypeAnnotation),
    TypeAnnotation,
    TypeExpression
      ( IntTypeExpression,
        FloatTypeExpression,
        CharTypeExpression,
        StringTypeExpression,
        BoolTypeExpression,
        NilTypeExpression,
        FunctionTypeExpression,
        RecordUnionTypeExpression,
        IdentifierTypeExpression
      ),
    TypeExpressionData,
    getTypeExpressionData,
  )
where

import Core.Utils
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

-- Module
data Module phase = Module (ModuleData phase) (MainFunction phase)

type family ModuleData phase

instance
  ( Pretty (MainFunction phase)
  ) =>
  Pretty (Module phase)
  where
  pretty (Module _ mainFunction) = "(Module " ++ pretty mainFunction ++ ")"

-- MainFunction
data MainFunction phase = MainFunction (MainFunctionData phase) (Scope phase)

type family MainFunctionData phase

instance
  ( Pretty (Scope phase)
  ) =>
  Pretty (MainFunction phase)
  where
  pretty (MainFunction _ scope) = "(MainFunction " ++ pretty scope ++ ")"

-- Scope
data Scope phase = Scope (ScopeData phase) (Seq (NonPositionalStatement phase)) (Seq (Statement phase))

type family ScopeData phase

getScopeData :: Scope phase -> ScopeData phase
getScopeData (Scope d _ _) = d

instance (Pretty (NonPositionalStatement phase), Pretty (Statement phase)) => Pretty (Scope phase) where
  pretty (Scope _ nonPositionalStatements statements) = "(Scope " ++ pretty nonPositionalStatements ++ " " ++ pretty statements ++ ")"

-- FunctionDefinition
data FunctionDefinition phase
  = FunctionDefinition
      (FunctionDefinitionData phase)
      (Seq (WithTypeAnnotation phase (ValueIdentifier phase)))
      (WithTypeAnnotation phase (Expression phase))

type family FunctionDefinitionData phase

instance
  ( Pretty (ValueIdentifier phase),
    Pretty (TypeAnnotation phase),
    Pretty (Expression phase)
  ) =>
  Pretty (FunctionDefinition phase)
  where
  pretty (FunctionDefinition _ parameters body) = "(FunctionDefinition (" ++ pretty parameters ++ ") " ++ pretty body ++ ")"

getFunctionDefinitionData :: FunctionDefinition phase -> FunctionDefinitionData phase
getFunctionDefinitionData (FunctionDefinition d _ _) = d

-- Statement
data Statement phase
  = PrintStatement (StatementData phase) (Expression phase)
  | VariableDeclarationStatement (StatementData phase) Mutability (WithTypeAnnotation phase (ValueIdentifier phase)) (Expression phase)
  | VariableMutationStatement (StatementData phase) (ValueIdentifier phase) (Expression phase)
  | FieldMutationStatement (StatementData phase) (Expression phase) (FieldIdentifier phase) (Expression phase)
  | ExpressionStatement (StatementData phase) (Expression phase)
  | {- The body of a while loop statement could really be a statement rather than an expression. However, this would
      require a while loop statement to create a new scope for its body. For now, to avoid having to implement the
      handling for this, I'm just making the body an expression instead. I expect that the body of a while loop is usually
      a scope expression, so there isn't too much practical difference either way.
    -}
    WhileLoopStatement (StatementData phase) (Expression phase) (Expression phase)
  | ReturnStatement (StatementData phase) (Maybe (Expression phase))

type family StatementData phase

data Mutability = Mutable | Immutable deriving (Show, Eq, Ord)

instance Pretty Mutability where
  pretty = show

instance
  ( Pretty (TypeAnnotation phase),
    Pretty (Expression phase),
    Pretty (ValueIdentifier phase),
    Pretty (FieldIdentifier phase)
  ) =>
  Pretty (Statement phase)
  where
  pretty (PrintStatement _ expression) = "(PrintStatement " ++ pretty expression ++ ")"
  pretty (VariableDeclarationStatement _ mutability variableName value) =
    "(VariableDeclarationStatement " ++ pretty mutability ++ " " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (VariableMutationStatement _ variableName value) = "(VariableMutationStatement " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (FieldMutationStatement _ record fieldName value) =
    "(FieldMutationStatement " ++ pretty record ++ " " ++ pretty fieldName ++ " " ++ pretty value ++ ")"
  pretty (ExpressionStatement _ expression) = "(ExpressionStatement " ++ pretty expression ++ ")"
  pretty (WhileLoopStatement _ condition statement) = "(WhileLoopStatement " ++ pretty condition ++ " " ++ pretty statement ++ ")"
  pretty (ReturnStatement _ (Just expression)) = "(ReturnStatement " ++ pretty expression ++ ")"
  pretty (ReturnStatement _ Nothing) = "(ReturnStatement)"

getStatementData :: Statement phase -> StatementData phase
getStatementData (PrintStatement d _) = d
getStatementData (VariableDeclarationStatement d _ _ _) = d
getStatementData (VariableMutationStatement d _ _) = d
getStatementData (FieldMutationStatement d _ _ _) = d
getStatementData (ExpressionStatement d _) = d
getStatementData (WhileLoopStatement d _ _) = d
getStatementData (ReturnStatement d _) = d

-- NonPositionalStatement
data NonPositionalStatement phase
  = FunctionStatement (NonPositionalStatementData phase) (FunctionIdentifier phase) (FunctionDefinition phase)
  | RecordStatement
      (NonPositionalStatementData phase)
      (RecordIdentifier phase)
      (Maybe (MutabilityParameter phase))
      (Seq (TypeParameter phase))
      (Seq (FieldIdentifier phase, TypeExpression phase))

type family NonPositionalStatementData phase

instance
  ( Pretty (FunctionIdentifier phase),
    Pretty (FunctionDefinition phase),
    Pretty (RecordIdentifier phase),
    Pretty (FieldIdentifier phase),
    Pretty (TypeExpression phase),
    Pretty (TypeParameter phase),
    Pretty (MutabilityParameter phase)
  ) =>
  Pretty (NonPositionalStatement phase)
  where
  pretty (FunctionStatement _ functionName definition) = "(FunctionStatement " ++ pretty functionName ++ " " ++ pretty definition ++ ")"
  pretty (RecordStatement _ recordName mutabilityParameter typeParameters fieldTypes) =
    "(RecordStatement " ++ pretty recordName ++ " " ++ pretty mutabilityParameter ++ " " ++ pretty typeParameters ++ " " ++ pretty fieldTypes ++ ")"

-- Identifier
type family Identifier phase

type family ValueIdentifier phase

type family FunctionIdentifier phase

type family FieldIdentifier phase

type family RecordIdentifier phase

type family TypeIdentifier phase

type family TypeParameter phase

type family MutabilityParameter phase

-- Expression
data Expression phase
  = IntLiteralExpression (ExpressionData phase) Int
  | FloatLiteralExpression (ExpressionData phase) Double
  | CharLiteralExpression (ExpressionData phase) Char
  | StringLiteralExpression (ExpressionData phase) Text
  | BoolLiteralExpression (ExpressionData phase) Bool
  | NilExpression (ExpressionData phase)
  | IdentifierExpression (ExpressionData phase) (Identifier phase)
  | NegateExpression (ExpressionData phase) (Expression phase)
  | AddExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | SubtractExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | MultiplyExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | DivideExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | ModuloExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | NotExpression (ExpressionData phase) (Expression phase)
  | AndExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | OrExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | EqualExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | NotEqualExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | GreaterExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | LessExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | GreaterEqualExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | LessEqualExpression (ExpressionData phase) (Expression phase) (Expression phase)
  | IfThenElseExpression (ExpressionData phase) (Expression phase) (Expression phase) (Maybe (Expression phase))
  | ScopeExpression (ExpressionData phase) (Scope phase)
  | FunctionExpression (ExpressionData phase) (FunctionDefinition phase)
  | FunctionCallExpression (ExpressionData phase) (Expression phase) (Seq (Expression phase))
  | RecordExpression (ExpressionData phase) Mutability (RecordIdentifier phase) (TypeArguments phase) (RecordFieldValues phase)
  | FieldAccessExpression (ExpressionData phase) (Expression phase) (FieldIdentifier phase)
  | CaseExpression (ExpressionData phase) (Expression phase) (Map (RecordIdentifier phase) (ValueIdentifier phase, Expression phase))

type family ExpressionData phase

type family RecordFieldValues phase

type family TypeArguments phase

instance
  ( Pretty (FunctionDefinition phase),
    Pretty (TypeAnnotation phase),
    Pretty (Identifier phase),
    Pretty (Scope phase),
    Pretty (RecordIdentifier phase),
    Pretty (FieldIdentifier phase),
    Pretty (RecordFieldValues phase),
    Pretty (ValueIdentifier phase),
    Pretty (TypeExpression phase),
    Pretty (TypeArguments phase)
  ) =>
  Pretty (Expression phase)
  where
  pretty (IntLiteralExpression _ value) = "(IntLiteralExpression " ++ show value ++ ")"
  pretty (FloatLiteralExpression _ value) = "(FloatLiteralExpression " ++ show value ++ ")"
  pretty (CharLiteralExpression _ value) = "(CharLiteralExpression " ++ show value ++ ")"
  pretty (StringLiteralExpression _ value) = "(StringLiteralExpression " ++ show value ++ ")"
  pretty (BoolLiteralExpression _ value) = "(BoolLiteralExpression " ++ show value ++ ")"
  pretty (NilExpression _) = "(NilExpression)"
  pretty (IdentifierExpression _ identifier) = "(IdentifierExpression " ++ pretty identifier ++ ")"
  pretty (NegateExpression _ inner) = "(NegateExpression " ++ pretty inner ++ ")"
  pretty (AddExpression _ left right) = "(AddExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (SubtractExpression _ left right) = "(SubtractExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (MultiplyExpression _ left right) = "(MultiplyExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (DivideExpression _ left right) = "(DivideExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (ModuloExpression _ left right) = "(ModuloExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (NotExpression _ inner) = "(NotExpression " ++ pretty inner ++ ")"
  pretty (AndExpression _ left right) = "(AndExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (OrExpression _ left right) = "(OrExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (EqualExpression _ left right) = "(EqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (NotEqualExpression _ left right) = "(NotEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (GreaterExpression _ left right) = "(GreaterExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (LessExpression _ left right) = "(LessExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (GreaterEqualExpression _ left right) = "(GreaterEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (LessEqualExpression _ left right) = "(LessEqualExpression " ++ pretty left ++ " " ++ pretty right ++ ")"
  pretty (IfThenElseExpression _ condition trueExpression Nothing) =
    "(IfThenElseExpression " ++ pretty condition ++ " " ++ pretty trueExpression ++ ")"
  pretty (IfThenElseExpression _ condition trueExpression (Just falseExpression)) =
    "(IfThenElseExpression " ++ pretty condition ++ " " ++ pretty trueExpression ++ " " ++ pretty falseExpression ++ ")"
  pretty (ScopeExpression _ scope) = "(ScopeExpression " ++ pretty scope ++ ")"
  pretty (FunctionExpression _ definition) = "(FunctionExpression" ++ pretty definition ++ ")"
  pretty (FunctionCallExpression _ function arguments) = "(FunctionCallExpression " ++ pretty function ++ " (" ++ pretty arguments ++ "))"
  pretty (RecordExpression _ mutability recordName typeArguments fields) =
    "(RecordExpression " ++ pretty mutability ++ " " ++ pretty recordName ++ " " ++ pretty typeArguments ++ " " ++ pretty fields ++ ")"
  pretty (FieldAccessExpression _ inner field) = "(FieldAccessExpression " ++ pretty inner ++ " " ++ pretty field ++ ")"
  pretty (CaseExpression _ switch cases) = "(CaseExpression " ++ pretty switch ++ " " ++ pretty cases ++ ")"

getExpressionData :: Expression phase -> ExpressionData phase
getExpressionData (IntLiteralExpression d _) = d
getExpressionData (FloatLiteralExpression d _) = d
getExpressionData (CharLiteralExpression d _) = d
getExpressionData (StringLiteralExpression d _) = d
getExpressionData (BoolLiteralExpression d _) = d
getExpressionData (NilExpression d) = d
getExpressionData (IdentifierExpression d _) = d
getExpressionData (NegateExpression d _) = d
getExpressionData (AddExpression d _ _) = d
getExpressionData (SubtractExpression d _ _) = d
getExpressionData (MultiplyExpression d _ _) = d
getExpressionData (DivideExpression d _ _) = d
getExpressionData (ModuloExpression d _ _) = d
getExpressionData (NotExpression d _) = d
getExpressionData (AndExpression d _ _) = d
getExpressionData (OrExpression d _ _) = d
getExpressionData (EqualExpression d _ _) = d
getExpressionData (NotEqualExpression d _ _) = d
getExpressionData (GreaterExpression d _ _) = d
getExpressionData (LessExpression d _ _) = d
getExpressionData (GreaterEqualExpression d _ _) = d
getExpressionData (LessEqualExpression d _ _) = d
getExpressionData (IfThenElseExpression d _ _ _) = d
getExpressionData (ScopeExpression d _) = d
getExpressionData (FunctionExpression d _) = d
getExpressionData (FunctionCallExpression d _ _) = d
getExpressionData (RecordExpression d _ _ _ _) = d
getExpressionData (FieldAccessExpression d _ _) = d
getExpressionData (CaseExpression d _ _) = d

-- Type annotation
data WithTypeAnnotation phase a = WithTypeAnnotation a (TypeAnnotation phase)

type family TypeAnnotation phase

instance (Pretty (TypeAnnotation phase), Pretty a) => Pretty (WithTypeAnnotation phase a) where
  pretty (WithTypeAnnotation inner annotation) = "(WithTypeAnnotation " ++ pretty inner ++ " " ++ pretty annotation ++ ")"

-- Type
data TypeExpression phase
  = IntTypeExpression (TypeExpressionData phase)
  | FloatTypeExpression (TypeExpressionData phase)
  | CharTypeExpression (TypeExpressionData phase)
  | StringTypeExpression (TypeExpressionData phase)
  | BoolTypeExpression (TypeExpressionData phase)
  | NilTypeExpression (TypeExpressionData phase)
  | FunctionTypeExpression (TypeExpressionData phase) (Seq (TypeExpression phase)) (TypeExpression phase)
  | RecordUnionTypeExpression (TypeExpressionData phase) (Either Mutability (MutabilityParameter phase)) (Seq (RecordIdentifier phase, Seq (TypeExpression phase)))
  | IdentifierTypeExpression (TypeExpressionData phase) (TypeIdentifier phase)

type family TypeExpressionData phase

instance
  ( Pretty (TypeIdentifier phase),
    Pretty (MutabilityParameter phase),
    Pretty (RecordIdentifier phase)
  ) =>
  Pretty (TypeExpression phase)
  where
  pretty (IntTypeExpression _) = "IntTypeExpression"
  pretty (FloatTypeExpression _) = "FloatTypeExpression"
  pretty (CharTypeExpression _) = "CharTypeExpression"
  pretty (StringTypeExpression _) = "StringTypeExpression"
  pretty (BoolTypeExpression _) = "BoolTypeExpression"
  pretty (NilTypeExpression _) = "NilTypeExpression"
  pretty (FunctionTypeExpression _ argumentTypes returnType) = "(FunctionTypeExpression (" ++ pretty argumentTypes ++ ") " ++ pretty returnType ++ ")"
  pretty (RecordUnionTypeExpression _ mutability records) = "(RecordUnionTypeExpression" ++ pretty mutability ++ " " ++ pretty records ++ ")"
  pretty (IdentifierTypeExpression _ identifier) = "(IdentifierTypeExpression " ++ pretty identifier ++ ")"

getTypeExpressionData :: TypeExpression phase -> TypeExpressionData phase
getTypeExpressionData (IntTypeExpression d) = d
getTypeExpressionData (FloatTypeExpression d) = d
getTypeExpressionData (CharTypeExpression d) = d
getTypeExpressionData (StringTypeExpression d) = d
getTypeExpressionData (BoolTypeExpression d) = d
getTypeExpressionData (NilTypeExpression d) = d
getTypeExpressionData (FunctionTypeExpression d _ _) = d
getTypeExpressionData (RecordUnionTypeExpression d _ _) = d
getTypeExpressionData (IdentifierTypeExpression d _) = d
