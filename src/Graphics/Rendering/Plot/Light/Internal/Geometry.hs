{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{- |
This module provides functionality for working with affine transformations (i.e. in the unit square)
 
-}
module Graphics.Rendering.Plot.Light.Internal.Geometry where

import Data.Monoid ((<>))



-- | A `Point` defines a point in R2
data Point a = Point { _px :: a,
                       _py :: a } deriving (Eq)

instance Show a => Show (Point a) where
  show (Point x y) = show x ++ "," ++ show y

mkPoint :: a -> a -> Point a
mkPoint = Point

-- | Overwrite either coordinate of a Point, to e.g. project on an axis
setPointCoord :: Axis -> a -> Point a -> Point a
setPointCoord axis c (Point x y)
  | axis == X = Point c y
  | otherwise = Point x c

setPointX, setPointY :: a -> Point a -> Point a
setPointX = setPointCoord X
setPointY = setPointCoord Y

-- | A `LabeledPoint` carries a "label" (i.e. any additional information such as a text tag, or any other data structure), in addition to position information. Data points on a plot are `LabeledPoint`s.
data LabeledPoint l a =
  LabeledPoint {
   _lp :: Point a,
   _lplabel :: l
   } deriving (Eq, Show)

mkLabeledPoint :: Point a -> l -> LabeledPoint l a
mkLabeledPoint = LabeledPoint

moveLabeledPoint :: (Point a -> Point b) -> LabeledPoint l a -> LabeledPoint l b
moveLabeledPoint f (LabeledPoint p l) = LabeledPoint (f p) l

-- | A frame, i.e. a bounding box for objects
data Frame a = Frame {
   _fpmin :: Point a,
   _fpmax :: Point a
   } deriving (Eq, Show)

mkFrame :: Point a -> Point a -> Frame a
mkFrame = Frame

-- | Frame corner coordinates
xmin, xmax, ymin, ymax :: Frame a -> a
xmin = _px . _fpmin
xmax = _px . _fpmax
ymin = _py . _fpmin
ymax = _py . _fpmax

-- | The `width` is the extent in the `x` direction and `height` is the extent in the `y` direction
width, height :: Num a => Frame a -> a
width f = xmax f - xmin f
height f = ymax f - ymin f



-- * Axis

data Axis = X | Y deriving (Eq, Show)

otherAxis :: Axis -> Axis
otherAxis X = Y
otherAxis _ = X



-- * Dataset


-- data Dataset l a = Dataset
--   {
--     dsdat :: [LabeledPoint l a]
--   , dsframe ::Frame a
--   } deriving (Eq, Show)

-- instance Monoid (Dataset l a) where





  



-- | V2 is a vector in R^2
data V2 a = V2 a a deriving (Eq, Show)

-- | Vectors form a monoid w.r.t. vector addition
instance Num a => Monoid (V2 a) where
  mempty = V2 0 0
  (V2 a b) `mappend` (V2 c d) = V2 (a + b) (c + d)

-- | Additive group :
-- 
-- > v ^+^ zero == zero ^+^ v == v
--
-- > v ^-^ v == zero
class AdditiveGroup v where
  -- | Identity element
  zero :: v
  -- | Group action ("sum")
  (^+^) :: v -> v -> v
  -- | Inverse group action ("subtraction")
  (^-^) :: v -> v -> v

-- | Vectors form an additive group
instance Num a => AdditiveGroup (V2 a) where
  zero = mempty
  (^+^) = mappend
  (V2 a b) ^-^ (V2 c d) = V2 (a - c) (b - d)

-- | Vector space : multiplication by a scalar quantity
class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  -- | Scalar multiplication
  (.*) :: Scalar v -> v -> v
  
instance Num a => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  n .* (V2 vx vy) = V2 (n*vx) (n*vy)

-- | Hermitian space : inner product
class VectorSpace v => Hermitian v where
  type InnerProduct v :: *
  -- | Inner product
  (<.>) :: v -> v -> InnerProduct v

instance Num a => Hermitian (V2 a) where
  type InnerProduct (V2 a) = a
  (V2 a b) <.> (V2 c d) = (a*c) + (b*d)

-- | Euclidean (L^2) norm
norm2 ::
  (Hermitian v, Floating n, n ~ (InnerProduct v)) => v -> n
norm2 v = sqrt $ v <.> v

-- | Normalize a V2 w.r.t. its Euclidean norm
normalize2 :: (InnerProduct v ~ Scalar v, Floating (Scalar v), Hermitian v) =>
     v -> v
normalize2 v = (1/norm2 v) .* v


-- | Create a V2 `v` from two endpoints p1, p2. That is `v` can be seen as pointing from `p1` to `p2`
v2fromEndpoints, (-.) :: Num a => Point a -> Point a -> V2 a
v2fromEndpoints (Point px py) (Point qx qy) = V2 (qx-px) (qy-py)
(-.) = v2fromEndpoints

-- | The origin of the axes, point (0, 0)
origin :: Num a => Point a
origin = Point 0 0


-- | A Mat2 can be seen as a linear operator that acts on points in the plane
data Mat2 a = Mat2 a a a a deriving (Eq, Show)

-- | Linear maps, i.e. linear transformations of vectors
class Hermitian v => LinearMap m v where
  -- | Matrix action, i.e. linear transformation of a vector
  (#>) :: m -> v -> v

-- | Multiplicative matrix semigroup ("multiplying" two matrices together)
class MultiplicativeSemigroup m where
  -- | Matrix product
  (##) :: m -> m -> m

instance Num a => MultiplicativeSemigroup (Mat2 a) where
  Mat2 a00 a01 a10 a11 ## Mat2 b00 b01 b10 b11 = Mat2 (a00*b00+a01*b10) (a00*b01+a01*b11) (a10*b00+a11*b10) (a10*b01+a11*b11)

instance Num a => LinearMap (Mat2 a) (V2 a) where
  (Mat2 a00 a01 a10 a11) #> (V2 vx vy) = V2 (a00 * vx + a01 * vy) (a10 * vx + a11 * vy)

-- | Create a diagonal matrix
diagMat2 :: Num a => a -> a -> Mat2 a
diagMat2 rx ry = Mat2 rx 0 0 ry

-- | Diagonal matrices in R2 behave as scaling transformations
data DiagMat2 a = DMat2 a a deriving (Eq, Show)


-- | The class of invertible linear transformations
class LinearMap m v => MatrixGroup m v where
  -- | Inverse matrix action on a vector
  (<\>) :: m -> v -> v
  
instance Num a => MultiplicativeSemigroup (DiagMat2 a) where
  DMat2 a b ## DMat2 c d = DMat2 (a*c) (b*d)

instance Num a => LinearMap (DiagMat2 a) (V2 a) where
  DMat2 d1 d2 #> V2 vx vy = V2 (d1 * vx) (d2 * vy)

-- | Diagonal matrices can always be inverted
instance Fractional a => MatrixGroup (DiagMat2 a) (V2 a) where
  DMat2 d1 d2 <\> V2 vx vy = V2 (vx / d1) (vy / d2)

-- | Build a V2 from a `Point` p (i.e. assuming the V2 points from the origin (0,0) to p)
v2fromPoint :: Num a => Point a -> V2 a
v2fromPoint p = origin -. p

-- | Move a point along a vector
movePoint :: Num a => V2 a -> Point a -> Point a
movePoint (V2 vx vy) (Point px py) = Point (px + vx) (py + vy)

-- | Move a `LabeledPoint` along a vector
moveLabeledPointV2 :: Num a => V2 a -> LabeledPoint l a -> LabeledPoint l a
moveLabeledPointV2 = moveLabeledPoint . movePoint


-- | The vector translation from a `Point` contained in a `Frame` onto the unit square
--
-- NB: we do not check that `p` is actually contained within the frame. This has to be supplied correctly by the user
toUnitSquare :: (Fractional a, MatrixGroup (Mat2 a) (V2 a)) =>
    Frame a -> Point a -> Point a
toUnitSquare from p = movePoint vmove p
  where
    mm = diagMat2 (width from) (height from)
    o1 = _fpmin from
    vmove = mm <\> (p -. o1)

-- | The vector translation from a `Point` `p` contained in the unit square onto a `Frame`
--
-- NB: we do not check that `p` is actually contained in [0,1] x [0,1], This has to be supplied correctly by the user
fromUnitSquare :: Num a => Frame a -> Point a -> Point a
fromUnitSquare to p = movePoint vmove p
  where
    mm = diagMat2 (width to) (height to)
    vo = v2fromPoint (_fpmin to)
    vmove = (mm #> v2fromPoint p) ^+^ vo


-- -- * HasFrame : things which have a bounding box 
-- class HasFrame v where
--   type UnitInterval v :: *
--   type FrameType v :: *
--   fromFrame :: v -> UnitInterval v
--   toFrame :: UnitInterval v -> v



-- | X-aligned unit vector
e1 :: Num a => V2 a
e1 = V2 1 0
-- | Y-aligned unit vector
e2 :: Num a => V2 a
e2 = V2 0 1



-- | Numerical equality 
class Eps a where
  -- | Comparison within numerical precision
  (~=) :: a -> a -> Bool

instance Eps Double where
  a ~= b = abs (a - b) <= 1e-12

instance Eps Float where
  a ~= b = abs (a - b) <= 1e-6

instance Eps (V2 Double) where
  v1 ~= v2 = norm2 (v1 ^-^ v2) <= 1e-8
  
instance Eps (V2 Float) where
  v1 ~= v2 = norm2 (v1 ^-^ v2) <= 1e-3






-- class Located v where
--   type Coords v :: *
--   position :: v -> Coords v
