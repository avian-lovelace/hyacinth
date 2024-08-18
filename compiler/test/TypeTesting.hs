module TypeTesting
  ( testTypeInfo,
    getTypeSynonymInfoByName,
    getRecordTypeInfoByName,
    getTypeSynonymVarianceFuncByName,
  )
where

import Core.ErrorState
import Core.Errors
import Core.Pretty
import Core.SyntaxTree
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import IdentifierBinding.IdentifierBinder
import IdentifierBinding.SyntaxTree
import Lexing.Lexer
import Parsing.Parser
import Sectioning.Sectioner
import Test.Hspec
import TypeChecking.TypeChecker
import TypeChecking.TypeChecking
import TypeChecking.Variance

testTypeInfo :: Text -> TypeChecker a -> (a -> Expectation) -> Expectation
testTypeInfo code getTypeInfo typeInfoExpectations = case typeInfoOrError of
  Success a -> typeInfoExpectations a
  Error es -> expectationFailure $ fold $ Seq.intersperse "\n" $ pretty <$> es
  where
    typeInfoOrError = do
      tokens <- lexText code
      sections <- sectionFile tokens
      pAst <- parseFile sections
      (_boundValueIdentifierCounter, _boundFunctionIdentifierCounter, ibAst) <- runIdentifierBinding pAst
      execErrorState initialTypeCheckingState $ do
        _ <- typeCheckModule ibAst
        getTypeInfo

getTypeSynonymInfoByName :: Text -> TypeChecker TypeSynonymTypeInfo
getTypeSynonymInfoByName typeSynonym = do
  typeSynonymTypeInfos <- typeSynonymTypeInfos <$> getState
  let typeSynonymTypeInfosWithName = filter (\(typeSynonymId, _) -> getTextName typeSynonymId == typeSynonym) $ Map.toList typeSynonymTypeInfos
  case typeSynonymTypeInfosWithName of
    [] -> throwError $ TestError ("No type synonyms found with name " ++ Text.unpack typeSynonym)
    [(_, Right typeSynonymTypeInfo)] -> return typeSynonymTypeInfo
    [(_, Left _)] -> throwError $ TestError ("Type synonym " ++ Text.unpack typeSynonym ++ " was unchecked after type checking")
    _ -> throwError $ TestError ("Multiple type synonyms found with name " ++ Text.unpack typeSynonym)

getTypeSynonymVarianceFuncByName :: Text -> TypeChecker (Mutability -> Seq Variance)
getTypeSynonymVarianceFuncByName typeSynonym = do
  typeSynonymVarianceFuncs <- typeSynonymVarianceFuncs <$> getState
  let typeSynonymVarianceFuncsWithName = filter (\(typeSynonymId, _) -> getTextName typeSynonymId == typeSynonym) $ Map.toList typeSynonymVarianceFuncs
  case typeSynonymVarianceFuncsWithName of
    [] -> throwError $ TestError ("No type synonyms found with name " ++ Text.unpack typeSynonym)
    [(_, Right typeSynonymVarianceFunc)] -> return typeSynonymVarianceFunc
    [(_, Left _)] -> throwError $ TestError ("Type synonym " ++ Text.unpack typeSynonym ++ " was unchecked after type checking")
    _ -> throwError $ TestError ("Multiple type synonyms found with name " ++ Text.unpack typeSynonym)

getRecordTypeInfoByName :: Text -> TypeChecker RecordTypeInfo
getRecordTypeInfoByName recordName = do
  recordTypeInfos <- recordTypeInfos <$> getState
  let recordTypeInfosWithName = filter (\(recordId, _) -> getTextName recordId == recordName) $ Map.toList recordTypeInfos
  case recordTypeInfosWithName of
    [] -> throwError $ TestError ("No records found with name " ++ Text.unpack recordName)
    [(_, recordTypeInfo)] -> return recordTypeInfo
    _ -> throwError $ TestError ("Multiple records found with name " ++ Text.unpack recordName)