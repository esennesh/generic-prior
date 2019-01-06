{-|
Module      : Control.Monad.Bayes.Grammar
Description : Probabilistic grammars over arbitrary data types
Copyright   : (c) Eli Sennesh, 2019
License     : MIT
Maintainer  : esennesh@ccis.neu.edu
Stability   : experimental
Portability : GHC
-}

module Control.Monad.Bayes.Grammar (
  Grammar (Pure, Choice, (:&)),
  measure,
  nonparametric,
  sample,
) where

import Control.Monad
import Control.Monad.Bayes.Class
import Data.Maybe
import GHC.Generics
import Numeric.Log

data Grammar f a where
  Pure :: f a -> (a -> Log Double) -> Grammar f a
  Choice :: [Grammar f a] -> Grammar f a
  (:&) :: Grammar f a -> Grammar f b -> Grammar f (a, b)

sample :: MonadSample m => Grammar m a -> m a
sample (Pure m _) = m
sample (Choice mas) = join . uniformD $ (sample <$> mas)
sample (fa :& fb) = liftM2 (,) (sample fa) (sample fb)

measure :: Grammar m a -> a -> Log Double
measure (Pure _ p) a = p a
measure (Choice (g:[])) a = measure g a
measure (Choice (g:gs)) a | length gs >= 1 = first + rest - (first * rest) where
  first = measure g a
  rest = measure (Choice gs) a
measure (ga :& gb) (a, b) = (measure ga a) * (measure gb b)

repeatM :: Monad m => m a -> m [a]
repeatM m = sequence . repeat $ m

nonparametric :: MonadSample m => Grammar m a -> m [a]
nonparametric g = repeatM (sample g)
