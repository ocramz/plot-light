module Data.Bifunctor.Pair (Pair(..), Mix2(..))where

import Data.Bifunctor


-- | === Pair 
data Pair a b = P a b deriving (Eq, Show)

instance Bifunctor Pair where
  bimap f g (P x y) = P (f x) (g y)

-- | Mix2 generalizes Bifunctor because each field is updated with both values  
class Mix2 p where
  {-# MINIMAL mix2 #-}
  mix2 :: (a -> b -> c) -> (a -> b -> d) -> p a b -> p c d
  mix2l :: (a -> y -> b) -> p a y -> p b y
  mix2l f = mix2 f (\_ y -> y)
  mix2r :: (x -> b -> c) -> p x b -> p x c
  mix2r g = mix2 const g

instance Mix2 Pair where
  mix2 f g (P x y) = P (f x y) (g x y)
