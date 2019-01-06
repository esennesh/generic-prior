{-|
Module      : Control.Monad.Bayes.Grammar
Description : Probabilistic grammars over arbitrary data types
Copyright   : (c) Eli Sennesh, 2019
License     : MIT
Maintainer  : esennesh@ccis.neu.edu
Stability   : experimental
Portability : GHC
-}

module Control.Monad.Bayes.Grammar where

import Control.Monad
import Control.Monad.Bayes.Class
import Data.Maybe
import GHC.Generics
import Numeric.Log

data Grammar f a where
  Pure :: f a -> Grammar f a
  Choice :: [Grammar f a] -> Grammar f a
  (:&) :: Grammar f a -> Grammar f b -> Grammar f (a, b)
  (:$) :: (a -> b) -> Grammar f a -> Grammar f b

instance Functor f => Functor (Grammar f) where
  fmap f (Pure fa) = Pure (f <$> fa)
  fmap f (Choice fas) = Choice (map (fmap f) fas)
  fmap f (fa :& fb) = f :$ (fa :& fb)
  fmap f (g :$ fa) = (f . g) :$ fa

sample :: MonadSample m => Grammar m a -> m a
sample (Pure m) = m
sample (Choice mas) = join . uniformD $ (sample <$> mas)
sample (fa :& fb) = liftM2 (,) (sample fa) (sample fb)
sample (f :$ fa) = f <$> sample fa

repeatM :: Monad m => m a -> m [a]
repeatM m = sequence . repeat $ m

nonparametric :: MonadSample m => Grammar m a -> m [a]
nonparametric g = repeatM (sample g)
