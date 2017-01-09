{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Parade where

import Control.Monad.Trans.Free
import Data.Map (Map)

data Card = Card Color Int
  deriving (Eq, Ord, Show)

data Color
  = Red
  | Green
  | Blue
  | Purple
  | Yellow
  | Black
  deriving (Eq, Ord, Show)

type Parade = [Card]
type Deck = [Card]
type Hand = [Card]

type Tableau = Map Color Int

data GameMasterState =
  GameMasterState Int [(Hand, Tableau)] Parade Deck
  deriving (Eq, Ord, Show)

data GameState =
  GameState Hand Tableau [Tableau] Parade Int
  deriving (Eq, Ord, Show)

data PlayResult
  = Round GameState
  | FinalRound GameState
  | GameOver Int [Int]

data MasterPlayResult
  = MasterRound GameMasterState
  | MasterFinalRound GameMasterState
  | MasterGameOver [Int]

data Action x
  = Play Card (PlayResult -> x)
  deriving (Functor)

type Player m a = GameState -> FreeT Action m a

play :: Monad m => Card -> FreeT Action m PlayResult
play c = liftF (Play c id)

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

