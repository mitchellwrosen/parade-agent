{-# LANGUAGE ScopedTypeVariables #-}

module Monad where

import Card
import Game

import Control.Monad.Trans.Free (FreeT, liftF)

-- | The result of making a play from the perspective of a single player.
data PlayResult
  = Round GameState
  -- ^ The round continued as normal.
  | FinalRound GameState
  -- ^ Your or another player triggered the final round condition on or after
  -- your previous move; the next (legal) move you make on the given game state
  -- will be your last.
  | GameOver Int [Int]
  -- ^ The game is over.

{-
-- | The result of making a play.
data MasterPlayResult
  = MasterRound GameMasterState
  | MasterFinalRound GameMasterState
  | MasterGameOver [Int]
-}


-- | The underlying player action functor.
data Action x
  = Play Card (PlayResult -> x)
  -- ^ Play a card.
  deriving (Functor)

-- | Play smart constructor
play :: Monad m => Card -> FreeT Action m PlayResult
play c = liftF (Play c id)

{-
dummyAI :: Monad m => Player m (Int, [Int])
dummyAI (GameState (x:xs) _ _ _ _) = do
  res <- play x
  case res of
    Round gs -> dummyAI gs
    FinalRound gs -> dummyAI gs
    GameOver i is -> return (i, is)

toPlayerState :: GameMasterState -> GameState
toPlayerState = undefined

runParade :: forall m a . Monad m => Player m a -> m [Int]
runParade p = go start (p $ toPlayerState start)
  where
    start :: GameMasterState
    start = GameMasterState 0 [(hand, mempty)]
                 parade (replicate 50 (Card Red 4))

    hand :: [Card]
    hand = replicate 5 (Card Red 4)

    parade :: [Card]
    parade = replicate 6 (Card Red 4)

    go :: GameMasterState -> FreeT Action m a -> m [Int]
    go gms@(GameMasterState cur hs p d) pl = do
      res <- runFreeT pl
      case res of
        -- FIXME: this is where we can send error messages
        Pure a -> error "shouldn't happen"
        Free fb ->
          case fb of
            Play c k
              | not (elem c h) -> go gms (k (Round gs))
              | otherwise ->
                case runPlay c of
                  MasterRound gms'      ->
                    go gms' (k $ Round (toPlayerState gms'))
                  MasterFinalRound gms' ->
                    go gms' (k $ FinalRound (toPlayerState gms'))
                  MasterGameOver rs@(res:[]) -> do
                    runFreeT (k (GameOver res []))
                    pure rs
      where
        gs@(GameState h _ _ _ _) = toPlayerState gms

        runPlay :: Card -> MasterPlayResult
        runPlay = undefined
          -- draw cards for the hand
          -- add to parade
          -- what from the old parade gets added to tableau
-}
