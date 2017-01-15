module Deck
  ( Deck
  , initialDeck
  , drawCard
  , drawCards
  ) where

import Card

import Control.Monad.Random
import System.Random.Shuffle

type Deck = [Card]

initialDeck :: MonadRandom m => m Deck
initialDeck = shuffleM (Card <$> colors <*> ranks)
 where
  colors :: [Color]
  colors = [minBound..maxBound]

  ranks :: [Rank]
  ranks = [0..10]

drawCard :: Deck -> (Maybe Card, Deck)
drawCard []     = (Nothing, [])
drawCard (c:cs) = (Just c, cs)

drawCards :: Int -> Deck -> ([Card], Deck)
drawCards = splitAt
