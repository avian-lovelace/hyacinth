{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lexing.TextWalker
  ( FileState
      ( FileState,
        text,
        position
      ),
    WithEOF
      ( Consumed,
        HitEOF
      ),
    initFileState,
    getPosition,
    peek,
    advance,
    matchNext,
    consumeIf,
    consumeWhile,
    consumeUntil,
    consumeString,
  )
where

import Control.Monad.State (State, get, put)
import Core.FilePositions
import qualified Data.Text as Text

data FileState = FileState
  { text :: Text.Text,
    position :: Position
  }

data WithEOF a = Consumed a | HitEOF
  deriving (Functor)

initFileState :: Text.Text -> FileState
initFileState text = FileState {text, position = initPosition}

getPosition :: State FileState Position
getPosition = get >>= (return . position)

peek :: State FileState (WithEOF Char)
peek = do
  fileState <- get
  case Text.uncons $ text fileState of
    Nothing -> return HitEOF
    Just (nextChar, _) -> return $ Consumed nextChar

advance :: State FileState (WithEOF ())
advance = do
  FileState {text, position = Position {line, col}} <- get

  case Text.uncons text of
    Nothing -> return HitEOF
    Just (curChar, newText) ->
      let newPosition = case curChar of
            '\n' -> Position {line = (line + 1), col = 0}
            _ -> Position {line, col = (col + 1)}
       in do
            put $ FileState {text = newText, position = newPosition}
            return $ Consumed ()

matchNext :: (Char -> Bool) -> State FileState (WithEOF (Bool))
matchNext condition = do
  peekResult <- peek
  return $ condition <$> peekResult

consumeIf :: (Char -> Bool) -> State FileState (WithEOF (Maybe Char))
consumeIf condition = do
  maybeNextChar <- peek
  case maybeNextChar of
    HitEOF -> return HitEOF
    Consumed nextChar ->
      if
        | condition nextChar -> do
            advance
            return $ Consumed $ Just nextChar
        | otherwise -> return $ Consumed Nothing

consumeWhile :: (Char -> Bool) -> State FileState Text.Text
consumeWhile condition = consumeWhileHelper Text.empty condition

consumeWhileHelper :: Text.Text -> (Char -> Bool) -> State FileState (Text.Text)
consumeWhileHelper acc condition = do
  maybeNextChar <- peek
  case maybeNextChar of
    HitEOF -> return acc
    Consumed nextChar ->
      if
        | condition nextChar -> do
            advance
            consumeWhileHelper (Text.snoc acc nextChar) condition
        | otherwise -> return acc

consumeUntil :: (Char -> Bool) -> State FileState (WithEOF Text.Text)
consumeUntil condition = do
  consumedText <- consumeWhile (not . condition)
  advanceResult <- advance
  case advanceResult of
    Consumed _ -> return $ Consumed consumedText
    HitEOF -> return HitEOF

consumeString :: String -> State FileState (Maybe Text.Text)
consumeString str = do
  FileState {text, position = Position {line, col}} <- get
  let strLength = length str
  let strText = Text.take strLength text
  let newText = Text.drop strLength text
  if Text.unpack strText == str
    then do
      let newCol = col + toInteger strLength
      put $ FileState {text = newText, position = Position {line, col = newCol}}
      return $ Just strText
    else return Nothing