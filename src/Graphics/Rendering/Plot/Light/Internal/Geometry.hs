-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# language TypeFamilies, FlexibleContexts #-}
{- |
This module provides functionality for working with affine transformations (i.e. in the unit square)
 
-}
module Graphics.Rendering.Plot.Light.Internal.Geometry where

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Types

import Data.Monoid

-- mapPointToViewbox ff fx fy xlens ylens figdat xmin xmax ymin ymax p = LabeledPoint t' (xlens p) p' 
--   where
--     t' = affine (fx . ff $ _xmin figdat) (fx . ff $ _xmax figdat) (fx xmin) (fx xmax) (fx $ xlens p)
--     p' = withAffine (1 -) (fy . ff $ _ymin figdat) (fy . ff $ _ymax figdat) ymin ymax (fy $ ylens p)




-- viaUnitSquare fx fy xmin xmax ymin ymax (LabeledPoint (x, y) l a) =
--   LabeledPoint c' l a where
--     c' = (viaUnitInterv fx xmin xmax x, viaUnitInterv fy ymin ymax y)


-- viaUnitInterv :: Fractional t => (t -> t) -> t -> t -> t -> t
-- viaUnitInterv f xmin xmax = from01 . f . to01
--   where
--     to01 x = (x - xmin)/xd
--     from01 x = (x * xd) + xmin
--     xd = xmax - xmin






data V2 a = V2 a a deriving (Eq, Show)

instance Num a => Monoid (V2 a) where
  mempty = V2 0 0
  (V2 a b) `mappend` (V2 c d) = V2 (a + b) (c + d)

class AdditiveGroup v where
  zero :: v
  (^+^) :: v -> v -> v
  (^-^) :: v -> v -> v

instance Num a => AdditiveGroup (V2 a) where
  zero = mempty
  (^+^) = mappend
  (V2 a b) ^-^ (V2 c d) = V2 (a-c) (b-d)

class Vectorspace v where
  type Scalar v :: *
  (.*) :: Scalar v -> v -> v

instance Num a => Vectorspace (V2 a) where
  type Scalar (V2 a) = a
  n .* (V2 vx vy) = V2 (n*vx) (n*vy)

class Hermitian v where
  type InnerProduct v :: *
  (<.>) :: v -> v -> InnerProduct v

instance Num a => Hermitian (V2 a) where
  type InnerProduct (V2 a) = a
  (V2 a b) <.> (V2 c d) = (a*c) + (b*d)
  

norm2 ::
  (Hermitian v, Floating (InnerProduct v)) => v -> InnerProduct v
norm2 v = sqrt $ v <.> v

normalize2 :: (InnerProduct v ~ Scalar v, Floating (Scalar v), Vectorspace v,
      Hermitian v) =>
     v -> v
normalize2 v = (1/norm2 v) .* v


mkV2 (Point px py) (Point qx qy) = V2 (qx-px) (qy-py)
