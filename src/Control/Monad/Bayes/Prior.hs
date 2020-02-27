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
  MonadPrior, MonadPrior1,
  PriorScore,
  prior,
  priorGrammar,
  priorProbability,
) where

import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Grammar
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
  default gPriors :: (GMonadPrior f, MonadSample m) => [m (f p)]
  gPriors = [gPrior]

instance GMonadPriorSum V1
instance GMonadPriorSum U1
instance MonadPrior a => GMonadPriorSum (K1 i a)
instance GMonadPrior f => GMonadPriorSum (M1 i t f)
instance (GMonadPrior a, GMonadPrior b) => GMonadPriorSum (a :*: b)

instance (GMonadPriorSum a, GMonadPriorSum b) => GMonadPriorSum (a :+: b) where
  gPriors = (map (L1 <$>) as) ++ (map (R1 <$>) bs) where
    as = gPriors
    bs = gPriors

instance (GMonadPriorSum a, GMonadPriorSum b) => GMonadPrior (a :+: b) where
  gPrior = join $ uniformD gPriors

class GPriorScoreSum f where
  gPriorProbabilities :: [f p -> Log Double]
  default gPriorProbabilities :: GPriorScore f => [f p -> Log Double]
  gPriorProbabilities = [gPriorProbability]

instance GPriorScoreSum V1
instance GPriorScoreSum U1
instance PriorScore a => GPriorScoreSum (K1 i a)
instance GPriorScore f => GPriorScoreSum (M1 i t f)
instance (GPriorScore a, GPriorScore b) => GPriorScoreSum (a :*: b)

instance (GPriorScoreSum a, GPriorScoreSum b) => GPriorScoreSum (a :+: b) where
  gPriorProbabilities = map gLeftPrior as ++ map gRightPrior bs where
    gLeftPrior f (L1 a) = f a
    gLeftPrior _ (R1 _) = 0.0
    gRightPrior _ (L1 _) = 0.0
    gRightPrior g (R1 b) = g b
    as = gPriorProbabilities
    bs = gPriorProbabilities

instance (GPriorScoreSum a, GPriorScoreSum b) => GPriorScore (a :+: b) where
  gPriorProbability x =
    Prelude.sum [f x | f <- priors] / (fromIntegral $ length priors) where
      priors = gPriorProbabilities

priorGrammar :: (MonadPrior a, MonadSample m, PriorScore a) => Grammar m () a
priorGrammar = Pure (const (Just prior)) (const priorProbability)

class MonadPrior1 f where
  prior1 :: (MonadPrior a, MonadSample m) => m (f a)
  default prior1 :: (Generic1 f, GMonadPrior1 (Rep1 f), MonadPrior a,
                     MonadSample m) => m (f a)
  prior1 = to1 <$> gPrior1

class GMonadPrior1 g where
  gPrior1 :: (MonadPrior p, MonadSample m) => m (g p)

class GMonadPriorSum1 g where
  gPriors1 :: (MonadPrior p, MonadSample m) => [m (g p)]
  default gPriors1 :: (GMonadPrior1 g, MonadPrior p, MonadSample m) => [m (g p)]
  gPriors1 = [gPrior1]

instance GMonadPrior1 Par1 where
  gPrior1 = Par1 <$> prior

instance GMonadPriorSum1 Par1

instance GMonadPrior1 f => GMonadPrior1 (Rec1 f) where
  gPrior1 = Rec1 <$> gPrior1

instance GMonadPrior1 f => GMonadPriorSum1 (Rec1 f)

instance GMonadPrior1 V1 where
  gPrior1 = error "Cannot sample from Void!"

instance GMonadPrior1 U1 where
  gPrior1 = return U1

instance MonadPrior a => GMonadPrior1 (K1 i a) where
  gPrior1 = K1 <$> prior

instance GMonadPrior1 f => GMonadPrior1 (M1 i t f) where
  gPrior1 = M1 <$> gPrior1

instance PriorScore1 f => GPriorScore1 (M1 i t f) where
  gPriorProbability1 (M1 f) = priorProbability1 f

instance (GMonadPrior1 a, GMonadPrior1 b) => GMonadPrior1 (a :*: b) where
  gPrior1 = do
    a <- gPrior1
    b <- gPrior1
    return (a :*: b)

instance GMonadPriorSum1 V1
instance GMonadPriorSum1 U1
instance MonadPrior a => GMonadPriorSum1 (K1 i a)
instance GMonadPrior1 f => GMonadPriorSum1 (M1 i t f)
instance (GMonadPrior1 a, GMonadPrior1 b) => GMonadPriorSum1 (a :*: b)

instance (GMonadPriorSum1 a, GMonadPriorSum1 b) =>
         GMonadPriorSum1 (a :+: b) where
  gPriors1 = (map (L1 <$>) as) ++ (map (R1 <$>) bs) where
    as = gPriors1
    bs = gPriors1

instance (GMonadPriorSum1 a, GMonadPriorSum1 b) => GMonadPrior1 (a :+: b) where
  gPrior1 = join $ uniformD gPriors1
