{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell       #-}

-- | Parade game types and game logic.

module Game
  ( Hand
  , Parade
  , RankSet
  , Tableau
  , Player
  , GameState
  , GameMasterState
  , StepResult
  , stepParade
  ) where

import Card
import Deck
import LensTH

import Data.IntSet (IntSet)
import Data.Map (Map)
import Lens.Micro
import Lens.Micro.TH

type Hand = [Card]

type Parade = [Card]

type RankSet = IntSet

type Tableau = Map Color RankSet

type PlayerIx = Int

data Player = Player
  { hand    :: Hand
  , tableau :: Tableau
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''Player

-- | The game state from the perspective of a single player.
data GameState = GameState
  { hand      :: Hand
  , tableau   :: Tableau
  , parade    :: Parade
  , opponents :: [Tableau] -- TODO: Include num cards in hand?
  , deckSize  :: Int
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''GameState

data GameMasterState = GameMasterState
  { playerIx :: PlayerIx
  , players  :: [Player]
  , parade   :: Parade
  , deck     :: Deck
  , lastTurn :: Maybe PlayerIx
  -- ^ The index of the player who gets to make the last play before the game
  -- ends. 'Nothing' initially, and set to 'Just' when a final-round condition
  -- is met.
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''GameMasterState

-- | Lens on the current player of a 'GameMasterState'
curPlayerL :: Lens' GameMasterState Player
curPlayerL =
  lens
    (\s -> players s !! playerIx s)
    (\s p -> s & playersL . ix (playerIx s) .~ p)

data StepResult
  = InvalidMove
  -- ^ The card being played was not in the current player's hand.
  | Round GameMasterState
  -- ^ The new round is not the final one.
  | FinalRound GameMasterState
  -- ^ The new round is the final one.
  | GameOver GameMasterState
  -- ^ The game is over.

stepParade :: Card -> GameMasterState -> StepResult
stepParade card state
  | not (elem card (state ^. curPlayerL . handL)) = InvalidMove
  | otherwise = stepParade' card state

stepParade' :: Card -> GameMasterState -> StepResult
stepParade' card state = undefined
 where
  newState :: GameMasterState
  newState = state
    { playerIx = newPlayerIx
    , players  = newPlayers
    , parade   = newParade
    , deck     = newDeck
    , lastTurn = newLastTurn
    }

  newPlayerIx :: PlayerIx
  newPlayerIx = undefined

  newPlayers :: [Player]
  newPlayers = undefined

  newParade :: Parade
  newParade = undefined

  newDeck :: Deck
  newDeck = undefined

  newLastTurn :: Maybe PlayerIx
  newLastTurn = undefined
