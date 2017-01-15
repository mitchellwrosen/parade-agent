{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell       #-}

-- | Parade game types and game logic.

module Game
  ( Hand
  , Parade
  , RankSet
  , Tableau
  , Player(..)
  , GameState(..)
  , GameMasterState(..)
  , StepResult(..)
  , stepParade
  ) where

import Card
import Deck
import LensTH
import Tableau

import Data.List (delete)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Sequence (Seq, (<|))
import Data.Set (Set)
import Lens.Micro.Platform

import qualified Data.Set as Set

type Hand = [Card]

type Parade = Seq Card

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
  { playerIx   :: PlayerIx
  , players    :: [Player]
  , numPlayers :: !Int
  , parade     :: Parade
  , deck       :: Deck
  , lastTurn   :: Maybe PlayerIx
  -- ^ The index of the player who gets to make the last play before the game
  -- ends. 'Nothing' initially, and set to 'Just' when a final-round condition
  -- is met.
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''GameMasterState

-- | Lens on the player at a specific index.
playerAtL :: PlayerIx -> Lens' GameMasterState Player
playerAtL n =
  lens
    (\s -> players s !! n)
    (\s p -> s & playersL . ix n .~ p)

curPlayer :: GameMasterState -> Player
curPlayer = view curPlayerL

-- | Lens on the current player.
curPlayerL :: Lens' GameMasterState Player
curPlayerL f s = playerAtL (playerIx s) f s

-- | View a 'GameMasterState' as a 'GameState' from the perspective of a player.
toPlayerState :: PlayerIx -> GameMasterState -> GameState
toPlayerState i state = GameState
  { hand      = hand (curPlayer state)
  , tableau   = tableau (curPlayer state)
  , parade    = parade state
  -- TODO: Cycle opponents list so next opponent is at the head
  , opponents = deleteIx (playerIx state) (players state)
  -- TODO: Cache deck length in GameMasterState
  , deckSize  = length (deck state)
  }
 where
  deleteIx :: [a] -> Int -> [a]
  deleteIx xs n | n < 0 = xs
  deleteix [] _ = []
  deleteIx (x:xs) 0 = xs
  deleteix (_:xs) n = deleteIx xs (n-1)

-- | View a 'GameMasterState' as a 'GameState' from the perspective of the
-- current player.
curPlayerState :: GameMasterState -> GameState
curPlayerState state = toPlayerState (playerIx state) state

-- | If a move is made, would it be the last?
isLastMove :: GameMasterState -> Bool
isLastMove state = lastTurn state == Just (playerIx state)

-- | Has the game already entered the final round?
isFinalRound :: GameMasterState -> Bool
isFinalRound = isJust . lastTurn


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

-- Precondition: play is valid.
stepParade' :: Card -> GameMasterState -> StepResult
stepParade' card state
  | isLastMove   state    = GameOver   newState
  | isFinalRound newState = FinalRound newState
  | otherwise             = Round      newState
 where
  newState :: GameMasterState
  newState = state
    & playerIxL  %~ updatePlayerIx
    & curPlayerL %~ updateCurPlayer -- Updates playersL
    & paradeL    .~ newParade
    & deckL      %~ updateDeck
    & lastTurnL  %~ updateLastTurn

  -- Unless this is the last player to move, the player index just gets bumped.
  updatePlayerIx :: PlayerIx -> PlayerIx
  updatePlayerIx n
    | isLastMove state = n
    | otherwise        = (n+1) `mod` numPlayers state

  updateCurPlayer :: Player -> Player
  updateCurPlayer player = player
    -- Delete the played card from the player's hand
    & handL %~ delete card
    -- Draw the top card of the deck (if any)
    & handL %~ drawCard
    -- Insert into the tableau all of the cards attracted from the parade
    & tableauL %~ insertAttractedCards
   where
    drawCard :: Hand -> Hand
    drawCard =
      case deck state of
        []    -> id
        (c:_) -> (c:)

    -- Insert all of the attracted cards into the tableau.
    insertAttractedCards :: Tableau -> Tableau
    insertAttractedCards =
      appEndo (foldMap (Endo . insertTableau) attractedCards)

  -- To calculate the new parade, walk over it, binning each card into either
  -- the new parade, or a set of cards to be removed from the parade and added
  -- to the current player's tableau.
  newParade :: Parade
  attractedCards :: Set Card
  (newParade, attractedCards) = foldr step (pure card, mempty) (parade' state)
   where
    step :: Card -> (Parade, Set Card) -> (Parade, Set Card)
    step c (p, cs)
      | card `attracts` c = (p, Set.insert c cs)
      | otherwise = (c <| p, cs)

    -- Wow DuplicateRecordFields... fail.
    parade' :: GameMasterState -> Parade
    parade' = parade

  updateDeck :: Deck -> Deck
  updateDeck = safeTail

  updateLastTurn :: Maybe PlayerIx -> Maybe PlayerIx
  updateLastTurn (Just n) = Just n
  updateLastTurn Nothing
    | cond1 || cond2 = Just (playerIx state)
    | otherwise = Nothing
   where
    -- Final round condition 1: deck was just exhausted
    cond1 :: Bool
    cond1 = null (deck newState)

    -- Final round condition 2: current player's tableau has a card of every
    -- color. Can't use 'curPlayerL' lens here, since the new state's index has
    -- already been updated.
    cond2 :: Bool
    cond2 = length (newState ^. playerAtL (playerIx state) . tableauL) == 6

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs
