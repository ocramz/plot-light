{-# LANGUAGE MultiParamTypeClasses #-}
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

class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  (.*) :: Scalar v -> v -> v

instance Num a => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  n .* (V2 vx vy) = V2 (n*vx) (n*vy)

class VectorSpace v => Hermitian v where
  type InnerProduct v :: *
  (<.>) :: v -> v -> InnerProduct v


instance Num a => Hermitian (V2 a) where
  type InnerProduct (V2 a) = a
  (V2 a b) <.> (V2 c d) = (a*c) + (b*d)


norm2 ::
  (Hermitian v, Floating n, n ~ (InnerProduct v)) => v -> n
norm2 v = sqrt $ v <.> v

normalize2 :: (InnerProduct v ~ Scalar v, Floating (Scalar v), Hermitian v) =>
     v -> v
normalize2 v = (1/norm2 v) .* v


-- | Create a V2 `v` from two endpoints p1, p2. That is `v` can be seen as pointing from `p1` to `p2`
mkV2fromEndpoints :: Num a => Point a t1 -> Point a t -> V2 a
mkV2fromEndpoints (Point px py _) (Point qx qy _) = V2 (qx-px) (qy-py)

(-.) :: Num a => Point a t1 -> Point a t -> V2 a
(-.) = mkV2fromEndpoints


origin :: a -> Point Integer a
origin = Point 0 0


-- | A Mat2 can be seen as a linear operator V2 -> V2
data Mat2 a = Mat2 a a a a deriving (Eq, Show)

class Hermitian v => LinearMap m v where
  (#>) :: m -> v -> v

instance Num a => LinearMap (Mat2 a) (V2 a) where
  (Mat2 a00 a01 a10 a11) #> (V2 vx vy) = V2 (a00 * vx + a01 * vy) (a10 * vx + a11 * vy)

-- | A diagonal matrix
diagMat2 :: Num a => a -> a -> Mat2 a
diagMat2 rx ry = Mat2 rx 0 0 ry

-- | Diagonal matrices in R2 behave as scaling transformations
data DiagMat2 a = DMat2 a a deriving (Eq, Show)

-- | The class of invertible linear transformations
class LinearMap m v => MatrixGroup m v where
  (<\>) :: m -> v -> v

-- | Diagonal matrices can always be inverted
instance Num a => LinearMap (DiagMat2 a) (V2 a) where
  DMat2 d1 d2 #> V2 vx vy = V2 (d1 * vx) (d2 * vy)
instance Fractional a => MatrixGroup (DiagMat2 a) (V2 a) where
  DMat2 d1 d2 <\> V2 vx vy = V2 (vx/d1) (vy/d2)


-- remapFrame f1 f2 x = undefined
--   where
--     vorig = x -. _fpmin f1
--     rx = _fpmax f1 - 

-- class Located v where
--   type Coords v :: *
--   position :: v -> Coords v
