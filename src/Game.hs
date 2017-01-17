{-# language TemplateHaskell #-}

-- | Parade game types and game logic.

module Game
  ( -- * Parade game state
    GameState(..)
  , initialGameState
  , PlayerView(..)
  , toPlayerView
  , curPlayerView
  , StepResult(..)
  , stepParade
    -- * Misc. types
  , Hand
  , Parade
  , RankSet
  , Tableau
  , Player(..)
  , PlayerIx
  ) where

import Card
import Deck
import LensTH
import Tableau

import Control.Monad
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (State, runState)
import Data.List (delete)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Sequence (Seq, (<|))
import Data.Set (Set)
import GHC.Exts (IsList(fromList))
import Lens.Micro.Platform

import qualified Control.Monad.State as State
import qualified Data.Set as Set

type Hand = [Card]

type Parade = Seq Card

type PlayerIx = Int

type NumPlayers = Int

data Player = Player
  { hand    :: Hand
  , tableau :: Tableau
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''Player

data GameState = GameState
  { playerIx   :: PlayerIx
  , players    :: [Player]
  , numPlayers :: !NumPlayers
  , parade     :: Parade
  , deck       :: Deck
  , lastTurn   :: Maybe PlayerIx
  -- ^ The index of the player who gets to make the last play before the game
  -- ends. 'Nothing' initially, and set to 'Just' when a final-round condition
  -- is met.
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''GameState

-- | Lens on the player at a specific index.
playerAtL :: PlayerIx -> Lens' GameState Player
playerAtL n =
  lens
    (\s -> players s !! n)
    (\s p -> s & playersL . ix n .~ p)

curPlayer :: GameState -> Player
curPlayer = view curPlayerL

-- | Lens on the current player.
curPlayerL :: Lens' GameState Player
curPlayerL f s = playerAtL (playerIx s) f s

-- | Initialize a 'GameState' for the given number of players.
initialGameState :: MonadRandom m => NumPlayers -> m GameState
initialGameState n = do
  d <- initialDeck

  let hs :: [Hand]
      p  :: Parade
      d' :: Deck
      ((hs, p), d') =
        -- Draw a bunch of cards in a little state monad
        flip runState d $ do
          hs_ <- replicateM n drawHand
          p_  <- drawParade
          pure (hs_, p_)

  pure (GameState
    { playerIx   = 0
    , players    = map (\h -> Player h mempty) hs
    , numPlayers = n
    , parade     = p
    , deck       = d'
    , lastTurn   = Nothing
    })
 where
  drawHand :: State Deck Hand
  drawHand = State.state (drawCards 5)

  drawParade :: State Deck Parade
  drawParade = fromList <$> State.state (drawCards 6)

-- | If a move is made, would it be the last?
isLastMove :: GameState -> Bool
isLastMove state = lastTurn state == Just (playerIx state)

-- | Has the game already entered the final round?
isFinalRound :: GameState -> Bool
isFinalRound = isJust . lastTurn


-- | The game state from the perspective of a single player.
data PlayerView = PlayerView
  { hand      :: Hand
  , tableau   :: Tableau
  , parade    :: Parade
  , opponents :: [Tableau] -- TODO: Include num cards in hand?
  , deckSize  :: Int
  } deriving (Eq, Ord, Show)
makeLensesWith elSuffixFields ''PlayerView

-- | View a 'GameState as a 'PlayerView from the perspective of a player.
toPlayerView :: PlayerIx -> GameState -> PlayerView
toPlayerView i state = PlayerView
  { hand      = view (playerAtL i . handL) state
  , tableau   = view (playerAtL i . tableauL) state
  , parade    = view paradeL state
  -- TODO: Cycle opponents list so next opponent is at the head
  , opponents = deleteIx i (state ^.. playersL . each . tableauL)
  -- TODO: Cache deck length in GameState
  , deckSize  = length (deck state)
  }
 where
  deleteIx :: Int -> [a] -> [a]
  deleteIx n xs | n < 0 = xs
  deleteIx _ [] = []
  deleteIx 0 (_:xs) = xs
  deleteIx n (_:xs) = deleteIx (n-1) xs

-- | View a 'GameState as a 'PlayerView from the perspective of the
-- current player.
curPlayerView :: GameState -> PlayerView
curPlayerView state = toPlayerView (playerIx state) state


data StepResult
  = InvalidMove
  -- ^ The card being played was not in the current player's hand.
  | Round GameState
  -- ^ The new round is not the final one.
  | FinalRound GameState
  -- ^ The new round is the final one.
  | GameOver GameState
  -- ^ The game is over.

stepParade :: Card -> GameState -> StepResult
stepParade card state
  | not (elem card (state ^. curPlayerL . handL)) = InvalidMove
  | otherwise = stepParade' card state

-- Precondition: play is valid.
stepParade' :: Card -> GameState -> StepResult
stepParade' card state
  | isLastMove   state    = GameOver   newState
  | isFinalRound newState = FinalRound newState
  | otherwise             = Round      newState
 where
  newState :: GameState
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
    & handL %~ updateHand
    -- Insert into the tableau all of the cards attracted from the parade
    & tableauL %~ insertAttractedCards
   where
    updateHand :: Hand -> Hand
    updateHand =
      case drawCard (deck state) of
        (Nothing, _) -> id
        (Just c,  _) -> (c:)

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
    parade' :: GameState -> Parade
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
