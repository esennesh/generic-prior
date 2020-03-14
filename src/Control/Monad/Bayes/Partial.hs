{-|
Module      : Control.Monad.Bayes.Partial
Description : Distributions over partially structured data of arbitrary types
Copyright   : (c) Eli Sennesh, 2020
License     : MIT
Maintainer  : esennesh@ccis.neu.edu
Stability   : experimental
Portability : GHC
-}

module Control.Monad.Bayes.Partial where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Prior
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Data.Functor.Fixedpoint
import Data.Maybe
import GHC.Generics
import Numeric.Log

instance MonadPrior1 f => MonadPrior (Fix f) where
  prior = Fix <$> prior1

instance PriorScore1 f => PriorScore (Fix f) where
  priorProbability (Fix f) = priorProbability1 f

type Failure f      = UFailure f IntVar
type BindingState f = IntBindingState f
type Binder f a     = (IntBindingT f Identity) (Either (Failure f) a)
type UExcept f      = Either (Failure f)

evalFBM :: Binder f a -> UExcept f a
evalFBM = runIdentity . evalIntBindingT

unifySample :: (Unifiable f, MonadPrior1 f, MonadSample m) => UTerm f IntVar ->
               m (Fix f)
unifySample (UVar _) = Fix <$> prior1
unifySample (UTerm t) = Fix <$> mapM unifySample t

unifyDensity :: (Unifiable f, PriorScore1 f) => UTerm f IntVar -> Fix f ->
                                                Log Double
unifyDensity template token = case binding of
  Left _ -> 0
  Right fs -> product $ map (maybe 0 priorProbability) fs
  where
    binding = evalFBM $ unifiers template (unfreeze token)

unifiers :: Unifiable f => UTerm f IntVar -> UTerm f IntVar ->
            (IntBindingT f Identity) (Either (Failure f) [Maybe (Fix f)])
unifiers x y = do
  x' <- runExceptT $ x =:= y
  case x' of
    Left failure   -> return $ Left failure
    Right solution -> let vars = foldr (\v vs -> v:vs) [] solution in do
      vals <- mapM lookupVar $ vars
      return . Right $ [join (freeze <$> val) | val <- vals]
