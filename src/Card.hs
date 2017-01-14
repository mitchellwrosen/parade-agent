-- | Parade card and related types.

module Card where

type Rank = Int

data Card = Card Color Rank
  deriving (Eq, Ord, Show)

data Color
  = Red
  | Green
  | Blue
  | Purple
  | Yellow
  | Black
  deriving (Eq, Ord, Show)
