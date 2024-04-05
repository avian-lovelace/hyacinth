module TokenTestHelpers (
  startRange,
  simpleRange,
) where

import Core.Utils

startRange :: Integer -> Range
startRange n = Range { start = Position { line = 1, col = 1 }, end = Position { line = 1, col = n + 1 }}

simpleRange :: Integer -> Integer -> Range
simpleRange startCol endCol = Range { start = Position { line = 1, col = startCol }, end = Position { line = 1, col = endCol }}