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
  adaptor,
  Grammar (Pure, Choice, (:&), (:|)),
  measure,
  refactoring,
  sample,
) where

import Control.Monad
import Control.Monad.Bayes.Class
import Data.Maybe
import GHC.Generics
import Numeric.Log

data Grammar f a b where
  Pure :: (a -> f b) -> (a -> b -> Log Double) -> Grammar f a b
  Choice :: [Grammar f a b] -> Grammar f a b
  (:&) :: Grammar f a b -> Grammar f a c -> Grammar f a (b, c)
  (:|) :: ((a -> Maybe (f b)), (a -> b -> Log Double)) -> Grammar f a b ->
          Grammar f a b

sample :: MonadSample m => Grammar m a b -> a -> m b
sample (Pure m _) a = m a
sample (Choice mas) a = join . uniformD $ [sample ma a | ma <- mas]
sample (fa :& fb) a = liftM2 (,) (sample fa a) (sample fb a)
sample ((ma, _) :| fb) a = case ma a of
  Just mb -> mb
  Nothing -> sample fb a

measure :: Grammar m a b -> a -> b -> Log Double
measure (Pure _ p) a b = p a b
measure (Choice gs) a b = measureChoice gs a b (length gs) where
  measureChoice [g] a b k = measure g a b / (fromIntegral k)
  measureChoice (g:gs) a b k = first + rest - first * rest where
    first = measureChoice [g] a b k
    rest = measureChoice gs a b k
measure (ga :& gb) a (b, c) = (measure ga a b) * (measure gb a c)
measure ((_, p) :| fb) a b = if density /= 0 then density else measure fb a b
  where
    density = p a b

repeatM :: Monad m => m a -> m [a]
repeatM m = sequence . repeat $ m

adaptor :: MonadSample m => Log Double -> Grammar m a b -> a -> m [b]
adaptor alpha g a = crpMem alpha (sample g a) id

refactoring :: MonadSample m => Log Double -> Grammar m a b -> a -> (b -> b) ->
               m [b]
refactoring alpha g a f = crpMem alpha (sample g a) f

crpMem :: MonadSample m => Log Double -> m a -> (a -> a) -> m [a]
crpMem alpha base transformMemo = draw [] where
  draw memo = drawBase memo >>= permute memo
  drawBase memo = let n = fromIntegral $ length memo in do
    r <- Exp . log <$> uniform 0 1
    if r < alpha / (n + alpha) then base else uniformD memo
  permute memo s = let sample = transformMemo s in do
    rest <- draw (sample:memo)
    return (sample:rest)
