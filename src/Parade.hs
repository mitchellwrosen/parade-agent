{-# LANGUAGE ScopedTypeVariables #-}

module Parade
  ( -- * Agents
    Agent
  , Action
  , play
  , PlayResult(..)
    -- * Agent runners
  , runParade
    -- * AIs
  , playFirstCardAI
  ) where

import Card
import Game

import Control.Monad
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Free
import Data.Map (Map, (!))

import qualified Data.Map as Map


type Agent = FreeT Action

-- | The underlying player action functor.
data Action x
  = Play Card (PlayResult -> x)
  -- ^ Play a card.
  deriving (Functor)

-- | The result of making a play from the perspective of a single player.
data PlayResult
  = PlayInvalidMove
  -- ^ The requested move was invalid.
  | PlayRound PlayerView
  -- ^ The round continued as normal.
  | PlayFinalRound PlayerView
  -- ^ Your or another player triggered the final round condition on or after
  -- your previous move; the next (legal) move you make on the given game state
  -- will be your last.
  | PlayGameOver PlayerView
  -- ^ The game is over.

-- | Play smart constructor
play :: Monad m => Card -> Agent m PlayResult
play c = liftF (Play c id)


-- | The tag of the previous move's 'StepResult', to be sent to the next player.
data StepResultType
  = TyInvalidMove
  | TyRound
  | TyFinalRound
  | TyGameOver

runParade :: forall m a. MonadRandom m => [PlayerView -> Agent m a] -> m [a]
runParade ps = do
  state <- initialGameState (length ps)

  let ps' :: Map PlayerIx (PlayResult -> Agent m a)
      ps' = Map.fromList (zip [0..] (map adjust ps))

  runParade' state TyRound ps'
 where
  -- | Adjust an "initial" player (expecting a 'PlayerView') to one that expects
  -- the result of a play (that it hasn't made yet), which *must* be a 'Round'.
  --
  -- This should be a total function under normal circumstances, since it
  -- shouldn't be possible for one's very first turn to be a 'FinalRound',
  -- unless there are so many players in the game that the entire deck's been
  -- exhausted.
  --
  -- It *definitely* shouldn't be possible to get an 'InvalidMove' or 'GameOver'
  -- 'PlayResult' no matter how many players are in the game.
  adjust :: (PlayerView -> Agent m a) -> (PlayResult -> Agent m a)
  adjust k = \case
    PlayRound state  -> k state
    PlayInvalidMove  -> error "Unexpected PlayInvalidMove"
    PlayFinalRound _ -> error "Unexpected PlayFinalRound"
    PlayGameOver   _ -> error "Unexpected PlayGameOver"

runParade'
  :: forall m a.
     Monad m
  => GameState
  -> StepResultType
  -> Map PlayerIx (PlayResult -> Agent m a)
  -> m [a]
runParade' state ty ps = do
  let -- Piece together the PlayResult to send to the next player from the
      -- result of the previous move. Return Nothing if the game's over.
      maybePlayRes :: Maybe PlayResult
      maybePlayRes =
        case ty of
          TyInvalidMove -> Just PlayInvalidMove
          TyRound       -> Just (PlayRound      (curPlayerView state))
          TyFinalRound  -> Just (PlayFinalRound (curPlayerView state))
          TyGameOver    -> Nothing

  case maybePlayRes of
    Nothing -> runParadeGameOver state ps
    Just playRes ->
      runFreeT (curPlayer playRes) >>= \case
        Pure _ -> error "Player logic ended early"
        Free (Play card k') ->
          case stepParade card state of
            InvalidMove         -> runParade' state    TyInvalidMove newPlayers
            Round      newState -> runParade' newState TyRound       newPlayers
            FinalRound newState -> runParade' newState TyFinalRound  newPlayers
            GameOver   newState -> runParade' newState TyGameOver    newPlayers
         where
          newPlayers :: Map PlayerIx (PlayResult -> Agent m a)
          newPlayers = Map.insert (playerIx state) k' ps
 where
  curPlayer :: PlayResult -> Agent m a
  curPlayer = ps ! playerIx state

-- | Send each player the 'GameOver' result, and expect their logic to end.
runParadeGameOver
  :: Monad m => GameState -> Map PlayerIx (PlayResult -> Agent m a) -> m [a]
runParadeGameOver state ps =
  forM (Map.assocs ps) $ \(i, k) ->
    runFreeT (k (PlayGameOver (toPlayerView i state))) >>= \case
      Pure a -> pure a
      Free _ -> error "Unexpected player logic"


--------------------------------------------------------------------------------
-- AIs

playFirstCardAI :: Monad m => PlayerView -> Agent m PlayerView
playFirstCardAI state = do
  let (card:_) = hand' state
  play card >>= \case
    PlayInvalidMove         -> error "playFirstCardAI: InvalidMove"
    PlayRound      newState -> playFirstCardAI newState
    PlayFinalRound newState -> playFirstCardAI newState
    PlayGameOver   newState -> pure newState
 where
  hand' :: PlayerView -> Hand
  hand' = hand
