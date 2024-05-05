{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.SyntaxTree
  ( Module (Module),
    ModuleData,
    ModuleContent,
    MainFunctionDefinition (MainFunctionDefinition),
    MainFunctionDefinitionData,
    FunctionDefinition (FunctionDefinition),
    FunctionDefinitionData,
    Statement
      ( PrintStatement,
        VariableDeclarationStatement,
        VariableMutationStatement,
        ExpressionStatement,
        WhileLoopStatement,
        ReturnStatement
      ),
    StatementData,
    getStatementData,
    Mutability (Mutable, Immutable),
    Identifier (Identifier),
    IdentifierData,
    IdentifierContent,
    getIdentifierName,
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
        VariableExpression,
        IfThenElseExpression,
        ScopeExpression,
        FunctionExpression,
        FunctionCallExpression
      ),
    ExpressionData,
    getExpressionData,
    FunctionExpressionContent,
    WithTypeAnnotation (WithTypeAnnotation),
    TypeAnnotation,
    TypeExpression
      ( IntTypeExpression,
        FloatTypeExpression,
        CharTypeExpression,
        StringTypeExpression,
        BoolTypeExpression,
        NilTypeExpression,
        FunctionTypeExpression
      ),
    TypeExpressionData,
    getTypeExpressionData,
    fromTypeExpression,
  )
where

import Core.Type
import Core.Utils
import Data.Sequence (Seq)
import Data.Text (Text)

-- Module
data Module phase = Module (ModuleData phase) (ModuleContent phase)

type family ModuleData phase

type family ModuleContent phase

instance
  ( Show (IdentifierContent phase),
    Pretty (FunctionExpressionContent phase),
    Pretty (ModuleContent phase)
  ) =>
  Pretty (Module phase)
  where
  pretty (Module _ content) = "(Module " ++ pretty content ++ ")"

-- MainFunctionDefinition
data MainFunctionDefinition phase = MainFunctionDefinition (MainFunctionDefinitionData phase) (Seq (Statement phase))

type family MainFunctionDefinitionData phase

instance
  ( Show (IdentifierContent phase),
    Pretty (FunctionExpressionContent phase),
    Pretty (ModuleContent phase),
    Pretty (TypeAnnotation phase)
  ) =>
  Pretty (MainFunctionDefinition phase)
  where
  pretty (MainFunctionDefinition _ statements) = "(MainFunctionDefinition " ++ pretty statements ++ ")"

-- FunctionDefinition
data FunctionDefinition phase
  = FunctionDefinition
      (FunctionDefinitionData phase)
      (Seq (WithTypeAnnotation phase (Identifier phase))) -- Parameters
      (Seq (Identifier phase)) -- Captured identifiers
      (WithTypeAnnotation phase (Expression phase)) -- body

type family FunctionDefinitionData phase

instance
  ( Show (IdentifierContent phase),
    Pretty (FunctionExpressionContent phase),
    Pretty (ModuleContent phase),
    Pretty (TypeAnnotation phase)
  ) =>
  Pretty (FunctionDefinition phase)
  where
  pretty (FunctionDefinition _ parameters capturedIdentifiers body) =
    "(FunctionDefinition (" ++ pretty parameters ++ ") (" ++ pretty capturedIdentifiers ++ ") " ++ pretty body ++ ")"

-- Statement
data Statement phase
  = PrintStatement (StatementData phase) (Expression phase)
  | VariableDeclarationStatement (StatementData phase) Mutability (WithTypeAnnotation phase (Identifier phase)) (Expression phase)
  | VariableMutationStatement (StatementData phase) (Identifier phase) (Expression phase)
  | ExpressionStatement (StatementData phase) (Expression phase)
  | {- The body of a while loop statement could really be a statement rather than an expression. However, this would
      require a while loop statement to create a new scope for its body. For now, to avoid having to implement the
      handling for this, I'm just making the body an expression instead. I expect that the body of a while loop is usually
      a scope expression, so there isn't too much practical difference either way.
    -}
    WhileLoopStatement (StatementData phase) (Expression phase) (Expression phase)
  | ReturnStatement (StatementData phase) (Maybe (Expression phase))

type family StatementData phase

data Mutability = Mutable | Immutable deriving (Show, Eq)

instance Pretty Mutability where
  pretty = show

instance
  ( Show (IdentifierContent phase),
    Pretty (FunctionExpressionContent phase),
    Pretty (TypeAnnotation phase)
  ) =>
  Pretty (Statement phase)
  where
  pretty (PrintStatement _ expression) = "(PrintStatement " ++ pretty expression ++ ")"
  pretty (VariableDeclarationStatement _ mutability variableName value) =
    "(VariableDeclarationStatement " ++ pretty mutability ++ " " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (VariableMutationStatement _ variableName value) = "(VariableMutationStatement " ++ pretty variableName ++ " " ++ pretty value ++ ")"
  pretty (ExpressionStatement _ expression) = "(ExpressionStatement " ++ pretty expression ++ ")"
  pretty (WhileLoopStatement _ condition statement) = "(WhileLoopStatement " ++ pretty condition ++ " " ++ pretty statement ++ ")"
  pretty (ReturnStatement _ (Just expression)) = "(ReturnStatement " ++ pretty expression ++ ")"
  pretty (ReturnStatement _ Nothing) = "(ReturnStatement)"

getStatementData :: Statement phase -> StatementData phase
getStatementData (PrintStatement d _) = d
getStatementData (VariableDeclarationStatement d _ _ _) = d
getStatementData (VariableMutationStatement d _ _) = d
getStatementData (ExpressionStatement d _) = d
getStatementData (WhileLoopStatement d _ _) = d
getStatementData (ReturnStatement d _) = d

-- Identifier
data Identifier phase = Identifier (IdentifierData phase) (IdentifierContent phase)

type family IdentifierData phase

type family IdentifierContent phase

instance (Show (IdentifierContent phase)) => Pretty (Identifier phase) where
  pretty (Identifier _ name) = "(Identifier " ++ show name ++ ")"

getIdentifierName :: Identifier phase -> IdentifierContent phase
getIdentifierName (Identifier _ identifierName) = identifierName

-- Expression
data Expression phase
  = IntLiteralExpression (ExpressionData phase) Int
  | FloatLiteralExpression (ExpressionData phase) Double
  | CharLiteralExpression (ExpressionData phase) Char
  | StringLiteralExpression (ExpressionData phase) Text
  | BoolLiteralExpression (ExpressionData phase) Bool
  | NilExpression (ExpressionData phase)
  | VariableExpression (ExpressionData phase) (Identifier phase)
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
  | ScopeExpression (ExpressionData phase) (Seq (Statement phase))
  | FunctionExpression (ExpressionData phase) (FunctionExpressionContent phase)
  | FunctionCallExpression (ExpressionData phase) (Expression phase) (Seq (Expression phase))

type family ExpressionData phase

type family FunctionExpressionContent phase

instance
  ( Show (IdentifierContent phase),
    Pretty (FunctionExpressionContent phase),
    Pretty (TypeAnnotation phase)
  ) =>
  Pretty (Expression phase)
  where
  pretty (IntLiteralExpression _ value) = "(IntLiteralExpression " ++ show value ++ ")"
  pretty (FloatLiteralExpression _ value) = "(FloatLiteralExpression " ++ show value ++ ")"
  pretty (CharLiteralExpression _ value) = "(CharLiteralExpression " ++ show value ++ ")"
  pretty (StringLiteralExpression _ value) = "(StringLiteralExpression " ++ show value ++ ")"
  pretty (BoolLiteralExpression _ value) = "(BoolLiteralExpression " ++ show value ++ ")"
  pretty (NilExpression _) = "(NilExpression)"
  pretty (VariableExpression _ variableName) = "(VariableExpression " ++ pretty variableName ++ ")"
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
  pretty (ScopeExpression _ statements) = "(ScopeExpression " ++ pretty statements ++ ")"
  pretty (FunctionExpression _ content) = "(FunctionExpression" ++ pretty content ++ ")"
  pretty (FunctionCallExpression _ function arguments) = "(FunctionCallExpression " ++ pretty function ++ " (" ++ pretty arguments ++ "))"

getExpressionData :: Expression phase -> ExpressionData phase
getExpressionData (IntLiteralExpression d _) = d
getExpressionData (FloatLiteralExpression d _) = d
getExpressionData (CharLiteralExpression d _) = d
getExpressionData (StringLiteralExpression d _) = d
getExpressionData (BoolLiteralExpression d _) = d
getExpressionData (NilExpression d) = d
getExpressionData (VariableExpression d _) = d
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

type family TypeExpressionData phase

instance Pretty (TypeExpression phase) where
  pretty (IntTypeExpression _) = "IntTypeExpression"
  pretty (FloatTypeExpression _) = "FloatTypeExpression"
  pretty (CharTypeExpression _) = "CharTypeExpression"
  pretty (StringTypeExpression _) = "StringTypeExpression"
  pretty (BoolTypeExpression _) = "BoolTypeExpression"
  pretty (NilTypeExpression _) = "NilTypeExpression"
  pretty (FunctionTypeExpression _ argumentTypes returnType) = "(FunctionTypeExpression (" ++ pretty argumentTypes ++ ") " ++ pretty returnType ++ ")"

getTypeExpressionData :: TypeExpression phase -> TypeExpressionData phase
getTypeExpressionData (IntTypeExpression d) = d
getTypeExpressionData (FloatTypeExpression d) = d
getTypeExpressionData (CharTypeExpression d) = d
getTypeExpressionData (StringTypeExpression d) = d
getTypeExpressionData (BoolTypeExpression d) = d
getTypeExpressionData (NilTypeExpression d) = d
getTypeExpressionData (FunctionTypeExpression d _ _) = d

fromTypeExpression :: TypeExpression phase -> Type
fromTypeExpression (IntTypeExpression _) = IntType
fromTypeExpression (FloatTypeExpression _) = FloatType
fromTypeExpression (CharTypeExpression _) = CharType
fromTypeExpression (StringTypeExpression _) = StringType
fromTypeExpression (BoolTypeExpression _) = BoolType
fromTypeExpression (NilTypeExpression _) = NilType
fromTypeExpression (FunctionTypeExpression _ parameterTypes returnType) = FunctionType (fromTypeExpression <$> parameterTypes) (fromTypeExpression returnType)