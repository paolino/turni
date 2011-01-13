{-# LANGUAGE ScopedTypeVariables #-}

module Shuffle (shuffle) where

import Control.Monad.Random (MonadRandom, getRandoms)
import Control.Applicative ((<$>))
import Data.List (sortBy)
import Data.Ord (comparing)

shuffle :: forall m a . (Functor m, MonadRandom m) => [a] -> m [a]
shuffle xs = map fst . sortBy (comparing snd) . zip xs <$> (getRandoms :: m [Integer])

