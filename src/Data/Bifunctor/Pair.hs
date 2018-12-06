module Data.Bifunctor.Pair (Pair(..), mkPair, Mix2(..))where

import Data.Bifunctor
import Data.Semigroup

-- | === Pair 
data Pair a b = P a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (P x a) = P x (f a)

mkPair :: a -> b -> Pair a b
mkPair = P

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (P u1 v1) <> (P u2 v2) = P (u1 <> u2) (v1 <> v2)  
  
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = P mempty mempty
  mappend = (<>)

instance Bifunctor Pair where
  bimap f g (P x y) = P (f x) (g y)

-- | Mix2 generalizes Bifunctor because each field is updated with both values  
class Bifunctor p => Mix2 p where
  {-# MINIMAL mix2 #-}
  mix2 :: (a -> b -> c) -> (a -> b -> d) -> p a b -> p c d
  mix2l :: (a -> y -> b) -> p a y -> p b y
  mix2l f = mix2 f (\_ y -> y)
  mix2r :: (x -> b -> c) -> p x b -> p x c
  mix2r g = mix2 const g

instance Mix2 Pair where
  mix2 f g (P x y) = P (f x y) (g x y)

-- | The Ord instance only compares the second component.
--
-- NB this is different from the Ord instance of a regular tuple
instance (Eq a, Eq vd, Ord a) => Ord (Pair vd a) where
  (P _ v1) <= (P _ v2) = v1 <= v2
