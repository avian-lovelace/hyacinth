module Core.Utils (
  Pretty(pretty),
  -- pretty,
  Position(Position, line, col),
  initPosition,
  Range(Range, start, end),
  Identifier,
  fromEither,
  seqStart,
  seqEnd,
  WithRange(getRange),
  getUnionRange
) where

import Data.Text(Text)
import Data.Sequence(Seq((:<|), (:|>)))

-- Classes
class Pretty t where
  pretty :: t -> String

-- Types
data Position = Position {
  line :: Integer,
  col :: Integer
} deriving (Show, Eq, Ord)

instance Pretty Position where
  pretty position = "(" ++ show (line position) ++ ", " ++ show (col position) ++ ")"

initPosition :: Position
initPosition = Position { line = 1, col = 1 }

data Range = Range {
  start :: Position,
  end :: Position
} deriving (Show, Eq)

instance Semigroup Range where
  -- | Get the smallest range containing both ranges
  (Range start1 end1) <> (Range start2 end2) = Range (min start1 start2) (max end1 end2)

instance Pretty Range where
  pretty range = "[" ++ pretty (start range) ++ " - " ++ pretty (end range) ++ "]"

class WithRange t where
  getRange :: t -> Range

instance WithRange Range where
  getRange = id

type Identifier = Text

getUnionRange :: WithRange r => [r] -> Range
getUnionRange (wr : wrs) = foldr (<>) (getRange wr) (getRange <$> wrs)
getUnionRange [] = undefined

-- Utility function
fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

seqStart :: Seq a -> a
seqStart (x :<| _) = x

seqEnd :: Seq a -> a
seqEnd (_ :|> x) = x