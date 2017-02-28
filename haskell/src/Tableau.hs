module Tableau where

import Card

import Data.Map (Map)
import Data.Monoid
import Data.IntSet (IntSet)

import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

type RankSet = IntSet

type Tableau = Map Color RankSet

-- | Insert a 'Card' into a 'Tableau'.
insertTableau :: Card -> Tableau -> Tableau
insertTableau card =
  Map.insertWith (<>) (color card) (IntSet.singleton (rank card))
