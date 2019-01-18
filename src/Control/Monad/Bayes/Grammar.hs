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
  adapt,
  Grammar (Pure, Choice, (:&)),
  measure,
  sample,
) where

import Control.Monad
import Control.Monad.Bayes.Class
import Data.Maybe
import GHC.Generics
import Numeric.Log

data Grammar f a b where
  Pure :: (a -> Maybe (f b)) -> (a -> b -> Log Double) -> Grammar f a b
  Choice :: [Grammar f a b] -> Grammar f a b
  (:&) :: Grammar f a b -> Grammar f a c -> Grammar f a (b, c)

sample :: MonadSample m => Grammar m a b -> a -> Maybe (m b)
sample (Pure m _) a = m a
sample (Choice mas) a = case catMaybes [sample ma a | ma <- mas] of
  [] -> Nothing
  mas -> Just . join . uniformD $ mas
sample (fa :& fb) a = case (sample fa a, sample fb a) of
  (Just ma, Just mb) -> Just $ liftM2 (,) ma mb
  _ -> Nothing

measure :: Grammar m a b -> a -> b -> Log Double
measure (Pure _ p) a b = p a b
measure (Choice gs) a b = measureChoice gs a b (length gs) where
  measureChoice [g] a b k = measure g a b / (fromIntegral k)
  measureChoice (g:gs) a b k = first + rest - first * rest where
    first = measureChoice [g] a b k
    rest = measureChoice gs a b k
measure (ga :& gb) a (b, c) = (measure ga a b) * (measure gb a c)

repeatM :: Monad m => m a -> m [a]
repeatM m = sequence . repeat $ m

adapt :: MonadSample m => Grammar m a b -> (forall b. m b -> m [b]) -> a ->
         Maybe (m [b])
adapt (Pure m _) adaptor a = adaptor <$> m a
adapt (Choice gs) adaptor a = case catMaybes [adapt g adaptor a | g <- gs] of
  [] -> Nothing
  mbs -> Just . join . uniformD $ mbs
adapt (ga :& gb) adaptor a = case (adapt ga adaptor a, adapt gb adaptor a) of
  (Just mb, Just mb') -> Just $ liftM2 zip mb mb'
  _ -> Nothing

crpMem :: MonadSample m => Log Double -> m a -> m [a]
crpMem alpha base = draw [] where
  draw memo = drawBase memo >>= permute memo
  drawBase memo = let n = fromIntegral $ length memo in do
    r <- Exp . log <$> uniform 0 1
    if r < alpha / (n + alpha) then base else uniformD memo
  permute memo s = do
    rest <- draw (s:memo)
    return (s:rest)
