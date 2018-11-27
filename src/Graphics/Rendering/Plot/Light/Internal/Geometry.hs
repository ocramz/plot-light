{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
{- |
This module provides functionality for working with affine transformations (i.e. in the unit square)
 
-}
module Graphics.Rendering.Plot.Light.Internal.Geometry
  (
  -- * Geometry
  midPoint, centerOfMass, 
  -- ** LabeledPoint
  LabeledPoint(..), mkLabeledPoint, labelPoint, mapLabel,
  -- ** Frame
  Frame(..), mkFrame, unitFrame, frameFromPoints, frameFromPointsWith, mkFrameOrigin,
  height, width, xmin, xmax, ymin, ymax, isPointInFrame, frameToAffine,
  frameDirac,
  -- ** Axis
  Axis(..), otherAxis,
  -- *** AxisData, AxisFrame
  AxisData(..), AxisFrame(..), -- axisX, axisY, mkAxisPoints,
  -- ** Vectors
  V2(..), mkV2, fromCartesian, _vxy, 
  -- pointFromV2,
  -- ** Matrices
  Mat2(..), DiagMat2(..), diagMat2, constDiagMat2, rotMtx, rescaleIso, 
  -- ** Primitive elements
  origin, oneOne, e1, e2, 
  -- ** Vector norm operations 
  norm2, normalize2,
  -- ** Vector projection
  projX, projY, 
  -- ** Operations on points
  pointRange, 
  -- ** Operations on vectors
  frameToFrame, frameToFrameValue, fromFrame, toFrame,
  -- ** Typeclasses
  AdditiveGroup(..), negateAG, VectorSpace(..), (./), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..),
  -- *** Numerical precision
  Eps(..), nearZero, 
  -- ** Utilities
  meshGrid, subdivSegment, interpolateBilinear
  )
where

-- import Data.Monoid ((<>))
import qualified Data.Foldable as F (toList)
import Control.Exception
import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Generics
import GHC.Real (Ratio(..))
import Data.Scientific
import Data.Semigroup (Semigroup(..))
import Data.Bifunctor



-- | V2 is a vector in R^2
data V2 a = V2 {_vx :: a, _vy :: a} deriving (Eq, Ord, Functor)

_vxy :: V2 a -> (a, a)
_vxy p = (_vx p, _vy p)

instance Show a => Show (V2 a) where
  show (V2 vx vy) = "(V2 "++ show vx ++", "++ show vy++")"

mkV2 :: a -> a -> V2 a
mkV2 = V2

-- | Build a V2 from its Cartesian components
fromCartesian :: Num a => a -> a -> V2 a
fromCartesian w h = (w .* e1) ^+^ (h .* e2)

instance Num a => Semigroup (V2 a) where
  (V2 a b) <> (V2 c d) = V2 (a + c) (b + d)  

-- | Vectors form a monoid w.r.t. vector addition
instance Num a => Monoid (V2 a) where
  mempty = V2 0 0
  mappend = (<>)

origin, oneOne :: Num a => V2 a
origin = mempty  
oneOne = mkV2 1 1

-- | Additive group :
-- 
-- > v ^+^ zero == zero ^+^ v == v
--
-- > v ^-^ v == zero
class Monoid v => AdditiveGroup v where
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

negateAG :: AdditiveGroup v => v -> v
negateAG v = zero ^-^ v  

-- | Vector space : multiplication by a scalar quantity
class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  -- | Scalar multiplication
  (.*) :: Scalar v -> v -> v


(./) :: (VectorSpace v, Fractional (Scalar v)) => v -> Scalar v -> v
v ./ n = recip n .* v

  
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
normalize2 v = (1 / norm2 v) .* v


-- -- | Create a V2 `v` from two endpoints p1, p2. That is `v` can be seen as pointing from `p1` to `p2`
-- v2fromEndpoints, (-.) :: Num a => Point a -> Point a -> V2 a
-- v2fromEndpoints p q = V2 (qx-px) (qy-py)
-- (-.) = v2fromEndpoints




-- | A Mat2 is a linear operator that acts on points in the plane to produce points on the plane.
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






-- | Diagonal matrices in R2 behave as scaling transformations
data DiagMat2 a = DMat2 a a deriving (Eq, Show)

instance Num a => Semigroup (DiagMat2 a) where
  (<>) = (##)

-- | Diagonal matrices form a monoid w.r.t. matrix multiplication and have the identity matrix as neutral element
instance Num a => Monoid (DiagMat2 a) where
  mempty = DMat2 1 1
  mappend = (<>)

instance Num a => Semigroup (Mat2 a) where
  (<>) = (##)

-- | Matrices form a monoid w.r.t. matrix multiplication and have the identity matrix as neutral element
instance Num a => Monoid (Mat2 a) where
  mempty = Mat2 1 0 0 1
  mappend = (<>)

-- | Create a diagonal matrix
diagMat2 :: Num a => a -> a -> DiagMat2 a
diagMat2 = DMat2

constDiagMat2 :: Num a => a -> DiagMat2 a
constDiagMat2 r = diagMat2 r r

rescaleIso :: (LinearMap (DiagMat2 a) v, Num a) => a -> v -> v
rescaleIso r v = constDiagMat2 r #> v


-- | Rotation matrix
rotMtx :: Floating a => a -> Mat2 a
rotMtx r = Mat2 (cos r) (- (sin r)) (sin r) (cos r)


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








-- | A `LabeledPoint` carries a "label" (i.e. any additional information such as a text tag, or any other data structure), in addition to position information. Data points on a plot are `LabeledPoint`s.
data LabeledPoint l a =
  LabeledPoint {
  -- | The coordinates of the `LabeledPoint` (i.e. where in the figure it will be rendered)
   _lp :: a,
   -- | Data associated with the `LabeledPoint`
   _lplabel :: l
   } deriving (Eq, Show, Functor)

instance Bifunctor LabeledPoint where
  bimap f g (LabeledPoint l a) = LabeledPoint (g l) (f a)


mkLabeledPoint :: a -> l -> LabeledPoint l a
mkLabeledPoint = LabeledPoint

-- | Given a labelling function and a `Point` `p`, returned a `LabeledPoint` containing `p` and the computed label
labelPoint :: (a -> l) -> a -> LabeledPoint l a
labelPoint lf p = LabeledPoint p (lf p)


-- | Apply a function to the label; uses the Bifunctor instance
mapLabel :: (l1 -> l2) -> LabeledPoint l1 a -> LabeledPoint l2 a
mapLabel = first




liftV2 :: (a -> a -> b) -> V2 a -> V2 a -> V2 b
liftV2 f (V2 xa ya) (V2 xb yb) = mkV2 (f xa xb) (f ya yb)

vInf, vSup :: Ord a => V2 a -> V2 a -> V2 a
vInf = liftV2 min
vSup = liftV2 max



midPoint :: (VectorSpace v, Fractional (Scalar v), Semigroup v, Monoid v) => v -> v -> v
midPoint u v = centerOfMass [u, v]

centerOfMass :: (Foldable t, VectorSpace v, Fractional (Scalar v), Semigroup v, Monoid v) =>
     t v -> v
centerOfMass vs = foldr ins mempty vs ./ n where
  ins a b = a <> b
  n = fromIntegral $ length vs



-- | A Frame is really just a pair of things
data Frame a = Frame {_fpmin :: a, _fpmax :: a} deriving (Eq, Show, Functor, Generic)

mkFrame :: a -> a -> Frame a
mkFrame = Frame

-- | The semigroup operation (`mappend`) applied on two `Frames` results in a new `Frame` that bounds both.
instance Ord a => Semigroup (Frame a) where
  (Frame p1min p1max) <> (Frame p2min p2max) = Frame (min p1min p2min) (max p1max p2max)

-- | A degenerate Frame (min and max points coincide)
frameDirac :: a -> Frame a
frameDirac p = mkFrame p p

instance (Monoid a, Ord a) => Monoid (Frame a) where
  mempty = Frame mempty mempty
  mappend = (<>)

unitFrame :: Num a => Frame (V2 a)
unitFrame = mkFrame origin oneOne
  
-- | Create a `Frame` from a container of `V2`s `P`, i.e. construct two points `p1` and `p2` such that :
--
-- p1 := inf(x,y) P
--
-- p2 := sup(x,y) P
-- frameFromPoints :: Ord a => [a] -> Frame a
-- frameFromPoints ps = foldr ins (frameDirac $ head ps) ps where
--   ins p fr = frameDirac p <> fr
frameFromPoints :: (Foldable t, Ord a) => t a -> Frame a
frameFromPoints = frameFromPointsWith id

frameFromPointsWith :: (Foldable t, Ord b) => (a -> b) -> t a -> Frame b
frameFromPointsWith f ps = foldr ins (frameDirac $ head psl) psl where
  ins p fr = frameDirac p <> fr
  psl = f <$> F.toList ps

  



isPointInFrame :: Ord a => Frame a -> a -> Bool
isPointInFrame (Frame p1 p2) p = p >= p1 && p <= p2


-- | Build a frame rooted at the origin (0, 0)
mkFrameOrigin :: Num a => a -> a -> Frame (V2 a)
mkFrameOrigin w h = mkFrame origin (mkV2 w h)

-- -- | The unit square (0, 0) - (1, 1)
-- unitFrame :: Num a => Frame a
-- unitFrame = mkFrame origin oneOne




 

-- | Frame corner coordinates
xmin, xmax, ymin, ymax :: Frame (V2 a) -> a
xmin = _vx . _fpmin
xmax = _vx . _fpmax
ymin = _vy . _fpmin
ymax = _vy . _fpmax

-- | The `width` is the extent in the `x` direction and `height` is the extent in the `y` direction
width, height :: Num a => Frame (V2 a) -> a
width f = abs $ xmax f - xmin f
height f = abs $ ymax f - ymin f




-- * Axis

-- super hacky, let's get rid of this

data Axis = X | Y deriving (Eq, Show)

otherAxis :: Axis -> Axis
otherAxis X = Y
otherAxis _ = X



-- data Linear
-- data Logarithmic


data AxisData a = AxisData {
    axisNIntervals :: Int   -- ^ Number of axis intervals
  , axisV :: V2 a           -- ^ Axis direction vector (Normalized)
  -- , axisOrigin :: Point a   -- ^ Axis origin
                           } deriving (Eq, Show)

-- axisLength :: Floating a => AxisData a -> a
-- axisLength (AxisData n v _) = fromIntegral n * norm2 v

-- -- | Create an X-aligned 'AxisData'
-- axisX :: Num a =>
--          Int
--       -> a  -- ^ Interval length 
--       -> Point a
--       -> AxisData a
-- axisX n ldx = AxisData n (ldx .* e1)

-- -- | Create an Y-aligned 'AxisData'
-- axisY :: Num a =>
--          Int
--       -> a  -- ^ Interval length
--       -> Point a -> AxisData a
-- axisY n ldy = AxisData n (ldy .* e2)

-- -- | Create the list of axis tick points from the 'AxisData'
-- mkAxisPoints :: (Num a, Enum a) => AxisData a -> [Point a]
-- mkAxisPoints (AxisData n v p0) =
--   map (\i -> movePoint (i .* v) p0) $ take n [0, 1 ..]

data AxisFrame a = AxisFrame {
    afFrame :: Frame a    -- ^ Position in the figure
  , afAxis1 :: AxisData a  -- ^ First axis
  , afAxis2 :: AxisData a  -- ^ Second axis
                             } deriving (Eq, Show)







-- | Interpolation

-- | Safe
-- interpolateBilinear  :: (Ord p, Fractional p, Show p) =>
--      Frame p -> (Point p -> p) -> Point p -> p
interpolateBilinear fr@(Frame p1 p2) f p
  | isPointInFrame fr p = interpolateBilinear' p1 p2 f p
  | otherwise = error $ unwords ["Point", show p, "is outside frame", show fr]

-- | Unsafe
-- interpolateBilinear' :: Fractional a => Point a -> Point a -> (Point a -> a) -> Point a -> a
interpolateBilinear' q11 q22 f p =
  let
    (x1, y1) = _vxy q11
    (x2, y2) = _vxy q22
    (x, y) = _vxy p
    q12 = mkV2 x1 y2
    q21 = mkV2 x2 y1
    fq11 = f q11
    fq22 = f q22
    fq12 = f q12
    fq21 = f q21
    den1 = (x1 - x2) * (y1 - y2)
    den2 = (x1 - x2) * (y2 - y1)
    c111 = fq11/den1
    c112 = fq11/den2
    c121 = fq12/den1
    c122 = fq12/den2
    c211 = fq21/den1
    c212 = fq21/den2
    c221 = fq22/den1
    c222 = fq22/den2
    a0 = c111 * x2 * y2 + c122 * x2 * y1 + c212 * x1 * y2 + c221 * x1 * y1
    a1 = c112 * y2      + c121 * y1      + c211 * y2      + c222 * y1
    a2 = c112 * x2      + c121 * x2      + c211 * x1      + c222 * x1
    a3 = c111           + c122           + c212           + c221
  in a0 + a1 * x + a2 * y + a3 * x * y












-- -- | Move a `LabeledPoint` along a vector
-- moveLabeledPointV2 :: Num a => V2 a -> LabeledPoint l a -> LabeledPoint l a
-- moveLabeledPointV2 = moveLabeledPoint . movePoint

-- moveLabeledPointV2 v = fmap (^+^ v)





-- | `pointRange n p q` returns a list of equi-spaced `Point`s between `p` and `q`.
-- pointRange :: (Fractional a, Integral n) =>
--      n -> Point a -> Point a -> [Point a]
pointRange n p q = [ (fromIntegral x .* vnth) ^+^ p | x <- [0 .. n]]
  where
    v = p ^-^ q
    vnth = (1/fromIntegral n) .* v







-- | A list of `nx` by `ny` points in the plane arranged on the vertices of a rectangular mesh.
--
-- | NB: Only the minimum x, y coordinate point is included in the output mesh. This is intentional, since the output from this can be used as an input to functions that use a corner rather than the center point as refernce (e.g. `rect`)
-- meshGrid
--   :: (Enum a, RealFrac a) =>
--      Frame a  
--   -> Int      -- ^ Number of points along x axis
--   -> Int      -- ^ " y axis
--   -> [Point a]
meshGrid (Frame p1 p2) nx ny = let
  (xmi, ymi) = _vxy p1
  (xma, yma) = _vxy p2
  in
    mkV2 <$> take nx (subdivSegment xmi xma nx) <*> take ny (subdivSegment ymi yma ny)

data MeshGrid a = MeshGrid (Frame a) Int Int deriving (Eq, Show, Generic)


meshGrid' :: (Real a1, RealFrac a, Enum a) => MeshGrid (V2 a1) -> [V2 a]
meshGrid' (MeshGrid frm nx ny) = meshGrid frm nx ny
  

subdivSegment :: (Real a, Enum b, RealFrac b) => a -> a -> Int -> [b]
subdivSegment x1 x2 n = f <$> [0, 1 ..] where
  xmin = min x1 x2
  xmax = max x1 x2
  l = fromRational $ toRational (xmax - xmin)
  f x  = x * l/fromIntegral n + fromRational (toRational xmin)


-- | Apply an affine transformation such that the resulting vector points to the unit square
fromFrame :: Fractional a => Frame (V2 a) -> V2 a -> V2 a
fromFrame from v = mfrom <\> (v ^-^ vfrom) where
  (mfrom, vfrom) = frameToAffine from 


-- | Apply an affine transformation to a vector that points within the unit square
toFrame :: Num a => Frame (V2 a) -> V2 a -> V2 a
toFrame to v01 = (mto #> v01) ^+^ vto where
  (mto, vto) = frameToAffine to



frameToAffine :: Num a => Frame (V2 a) -> (DiagMat2 a, V2 a)
frameToAffine frm = (m, v) where
  m = diagMat2 (width frm) (height frm)
  v = _fpmin frm

affineToFrame :: (Num a, LinearMap m (V2 a)) => m -> V2 a -> Frame (V2 a)
affineToFrame m v = mkFrame pmin pmax
  where
    p11 = mkV2 1 1
    v01 = origin ^-^ p11
    pmin = v
    pmax = v ^+^ (m #> v01)
    




-- | Given two frames `F1` and `F2`, returns a function `f` that maps an arbitrary vector `v` contained within `F1` onto one contained within `F2`.
--
-- This function is composed of three affine maps :
--
-- 1. map `v` into a vector `v01` that points within the unit square,
--
-- 2. map `v01` onto `v01'`. This transformation serves to e.g. flip the dataset along the y axis (since the origin of the SVG canvas is the top-left corner of the screen). If this is not needed one can just supply the identity matrix and the zero vector,
--
-- 3. map `v01'` onto the target frame `F2`. 
--
-- NB: we do not check that `v` is actually contained within the `F1`, nor that `v01'` is still contained within [0,1] x [0, 1]. This has to be supplied correctly by the user.
frameToFrame :: Fractional a => Frame (V2 a) -> Frame (V2 a) -> V2 a -> V2 a
frameToFrame fromf tof = toFrame tof . flipUD . fromFrame fromf 
  where
    flipUD :: Num a => V2 a -> V2 a
    flipUD (V2 vx vy) = V2 vx (1 - vy)         


-- -- | Map function values across frames
-- frameToFrameValue :: Fractional t =>
--       Frame t  -- ^ Initial frame
--    -> Frame t  -- ^ Final frame
--    -> t        -- ^ Initial value
--    -> t
frameToFrameValue from to x = (x01 * rto) + ymin to where
  x01 = (x - ymin from)/rfrom
  rfrom = height from
  rto = height to



-- moveLabeledPointBwFrames ::
--   Fractional a =>
--      Frame a          -- ^ Initial frame
--   -> Frame a          -- ^ Final frame
--   -> Bool             -- ^ Flip L-R in [0,1] x [0,1]
--   -> Bool             -- ^ Flip U-D in [0,1] x [0,1]
--   -> LabeledPoint l a -- ^ Initial `LabeledPoint`
--   -> LabeledPoint l a
-- moveLabeledPointBwFrames from to fliplr flipud lp = LabeledPoint p' (_lplabel lp)
--   where
--     vlp = v2fromPoint $ _lp lp -- vector associated with starting point
--     vlp' = frameToFrame from to fliplr flipud vlp -- vector associated w new point
--     p' = pointFromV2 vlp'



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

projX, projY :: Num a => V2 a -> V2 a
projX v = (v <.> e1) .* e1
projY v = (v <.> e2) .* e2



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
  v1 ~= v2 = norm2 (v1 ^-^ v2) <= 1e-2

nearZero :: (Num a, Eps a) => a -> Bool
nearZero x = x ~= 0










