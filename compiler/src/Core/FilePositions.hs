module Core.FilePositions
  ( Position (Position, line, col),
    initPosition,
    Range (Range, start, end),
    dummyRange,
    WithRange (getRange),
  )
where

import Core.Utils
import Data.Sequence (Seq)

-- Position
data Position = Position
  { line :: Integer,
    col :: Integer
  }
  deriving (Show, Eq, Ord)

instance Pretty Position where
  pretty position = "(" ++ show (line position) ++ ", " ++ show (col position) ++ ")"

initPosition :: Position
initPosition = Position {line = 1, col = 1}

-- Range
data Range = Range
  { start :: Position,
    end :: Position
  }
  deriving (Show, Eq)

instance Semigroup Range where
  -- Get the smallest range containing both ranges
  (Range start1 end1) <> (Range start2 end2) = Range (min start1 start2) (max end1 end2)

instance Pretty Range where
  pretty range = "[" ++ pretty (start range) ++ " - " ++ pretty (end range) ++ "]"

instance WithRange Range where
  getRange = id

dummyRange :: Range
dummyRange = Range (Position 0 0) (Position 0 0)

-- WithRange
class WithRange t where
  getRange :: t -> Range

instance (WithRange wr1, WithRange wr2) => WithRange (wr1, wr2) where
  getRange (r, s) = getRange r <> getRange s

instance (WithRange wr) => WithRange (Seq wr) where
  -- This function requires that the sequence is non-empty and sorted by position
  getRange wrs = getRange (seqHead wrs, seqTail wrs)