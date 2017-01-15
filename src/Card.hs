-- | Parade card and related types.

module Card where

type Rank = Int

data Card = Card
  { color :: Color
  , rank  :: Rank
  } deriving (Eq, Ord, Show)

data Color
  = Red
  | Green
  | Blue
  | Purple
  | Yellow
  | Black
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | @c1 `attracts` c2@ returns 'True' if @c2@ if of the same color or
-- equal-or-lesser rank as @c1@.
attracts :: Card -> Card -> Bool
attracts c1 c2 = color c1 == color c2 || rank c1 >= rank c2
