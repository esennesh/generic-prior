{-|
Module      : Control.Monad.Bayes.Prior
Description : Type-classes for priors over arbitrary data types
Copyright   : (c) Eli Sennesh, 2018
License     : MIT
Maintainer  : esennesh@ccis.neu.edu
Stability   : experimental
Portability : GHC
-}

module Control.Monad.Bayes.Prior (
  MonadPrior,
  prior
) where

import Control.Monad.Bayes.Class
import GHC.Generics
import Numeric.Log

class PriorScore a where
  priorProbability :: a -> Log Double

class MonadSample m => MonadPrior m a where
  prior :: m a

priorScore :: (MonadInfer m, MonadPrior m a, PriorScore a) => a -> m ()
priorScore = score . priorProbability
