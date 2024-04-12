module Core.FilePositions
  ( Position (Position, line, col),
    initPosition,
    Range (Range, start, end),
    WithRange (getRange),
    getUnionRange,
  )
where

import Core.Utils

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

-- WithRange
class WithRange t where
  getRange :: t -> Range

getUnionRange :: (WithRange r, WithRange s) => (r, s) -> Range
getUnionRange (r, s) = getRange r <> getRange s