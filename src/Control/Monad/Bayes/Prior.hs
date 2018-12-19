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
  GMonadPrior,
  GPriorScore
  --prior,
  --priorProbability,
) where

import Control.Monad.Bayes.Class
import GHC.Generics
import Numeric.Log

class MonadPrior a where
  prior :: MonadSample m => m a

class GMonadPrior f where
  gPrior :: (MonadSample m) => m (f p)

class GPriorScore f where
  gPriorProbability :: f p -> Log Double

instance GMonadPrior V1 where
  gPrior = error "Cannot sample from the empty sum-type!"

instance GPriorScore V1 where
  gPriorProbability _ = error "Cannot score under the empty sum-type!"

instance GMonadPrior U1 where
  gPrior = return U1

instance GPriorScore U1 where
  gPriorProbability U1 = 1

instance (GMonadPrior a, GMonadPrior b) => GMonadPrior (a :*: b) where
  gPrior = do
    a <- gPrior
    b <- gPrior
    return (a :*: b)

instance (GPriorScore a, GPriorScore b) => GPriorScore (a :*: b) where
  gPriorProbability (a :*: b) = gPriorProbability a * gPriorProbability b

class GMonadPriorSum f where
  gPriors :: MonadSample m => [m (f p)]

instance GMonadPriorSum V1 where
  gPriors = [gPrior]

instance GMonadPriorSum U1 where
  gPriors = [gPrior]

instance (GMonadPrior a, GMonadPrior b) => GMonadPriorSum (a :*: b) where
  gPriors = [gPrior]

instance (GMonadPriorSum a, GMonadPriorSum b) => GMonadPriorSum (a :+: b) where
  gPriors = (map (L1 <$>) as) ++ (map (R1 <$>) bs) where
    as = gPriors
    bs = gPriors

instance (GMonadPriorSum a, GMonadPriorSum b) => GMonadPrior (a :+: b) where
  gPrior = let priors = gPriors in do
    i <- uniformD [0..length priors - 1]
    priors !! i
