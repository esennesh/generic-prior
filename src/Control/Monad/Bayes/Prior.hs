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
  PriorScore
  --prior,
  --priorProbability,
) where

import Control.Monad.Bayes.Class
import GHC.Generics
import Numeric.Log

class MonadPrior a where
  prior :: MonadSample m => m a
  default prior :: (Generic a, GMonadPrior (Rep a), MonadSample m) => m a
  prior = to <$> gPrior

class GMonadPrior f where
  gPrior :: (MonadSample m) => m (f p)

class PriorScore a where
  priorProbability :: a -> Log Double
  default priorProbability :: (Generic a, GPriorScore (Rep a)) => a -> Log Double
  priorProbability = gPriorProbability . from

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

instance PriorScore a => GPriorScore (K1 i a) where
  gPriorProbability (K1 a) = priorProbability a

instance GPriorScore f => GPriorScore (M1 i t f) where
  gPriorProbability (M1 f) = gPriorProbability f

instance (GMonadPrior a, GMonadPrior b) => GMonadPrior (a :*: b) where
  gPrior = do
    a <- gPrior
    b <- gPrior
    return (a :*: b)

instance MonadPrior a => GMonadPrior (K1 i a) where
  gPrior = K1 <$> prior

instance GMonadPrior f => GMonadPrior (M1 i t f) where
  gPrior = M1 <$> gPrior

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

instance MonadPrior a => GMonadPriorSum (K1 i a) where
  gPriors = [gPrior]

instance GMonadPrior f => GMonadPriorSum (M1 i t f) where
  gPriors = [gPrior]

instance (GMonadPriorSum a, GMonadPriorSum b) => GMonadPrior (a :+: b) where
  gPrior = let priors = gPriors in do
    i <- uniformD [0..length priors - 1]
    priors !! i

class GPriorScoreSum f where
  gPriorProbabilities :: [f p -> Log Double]

instance GPriorScoreSum V1 where
  gPriorProbabilities = [gPriorProbability]

instance GPriorScoreSum U1 where
  gPriorProbabilities = [gPriorProbability]

instance (GPriorScore a, GPriorScore b) => GPriorScoreSum (a :*: b) where
  gPriorProbabilities = [gPriorProbability]

instance PriorScore a => GPriorScoreSum (K1 i a) where
  gPriorProbabilities = [gPriorProbability]

instance GPriorScore f => GPriorScoreSum (M1 i t f) where
  gPriorProbabilities = [gPriorProbability]

instance (GPriorScoreSum a, GPriorScoreSum b) => GPriorScoreSum (a :+: b) where
  gPriorProbabilities = map gLeftPrior as ++ map gRightPrior bs where
    gLeftPrior f (L1 a) = f a
    gLeftPrior _ (R1 _) = 0.0
    gRightPrior _ (L1 _) = 0.0
    gRightPrior g (R1 b) = g b
    as = gPriorProbabilities
    bs = gPriorProbabilities

instance (GPriorScoreSum a, GPriorScoreSum b) => GPriorScore (a :+: b) where
  gPriorProbability x = Prelude.sum [f x | f <- priors] / (fromIntegral $ length priors) where
    priors = gPriorProbabilities
