{-# language TypeFamilies, DeriveFunctor, FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import Data.Bifunctor
import Data.Bifunctor.Pair
import qualified Data.Foldable as F (toList)
import qualified Data.IntMap as IM

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Text.Blaze.Svg
import Text.Blaze.Svg.Renderer.String (renderSvg)





-- | =============
-- | A DSL for geometrical shapes

data Shp p vd v =
    C (ShapeCol p) vd v  -- ^ Circle
  | R (ShapeCol p) vd v  -- ^ Rectangle
  deriving (Eq, Show)

instance Bifunctor (Shp p) where
  bimap f g sh = case sh of
    C k vd v -> C k (f vd) (g v)
    R k vd v -> R k (f vd) (g v)

instance Mix2 (Shp p) where
  mix2 f g sh = case sh of
    C k vd v -> C k (f vd v) (g vd v)
    R k vd v -> R k (f vd v) (g vd v)

renderShape :: (Floating a, Real a) => Shp a (V2 a) (V2 a) -> Svg
renderShape sh = case sh of
  C col vd v -> circle r col v where r = norm2 vd
  R col vd v -> rect w h col v where (w, h) = _vxy vd



reposition :: (Foldable f, Ord a, Functor f, Fractional a) =>
     Frame (V2 a) -> f (Shp p (V2 a) (V2 a)) -> f (Shp p (V2 a) (V2 a))
reposition to shs = bias . frameToFrameB from to <$> shs where
  from = wrappingFrame shs

bias :: Num a => Shp p (V2 a) (V2 a) -> Shp p (V2 a) (V2 a)
bias sh = case sh of
  c@C{} -> c
  r@_ -> mix2r fbias r where
    fbias vd v = v ^-^ fromCartesian 0 (_vy vd)



wrappingFrame :: (Foldable t, AdditiveGroup v, Ord v) => t (Shp p v v) -> Frame v
wrappingFrame shs = foldr insf fzero ssh where
  (sh:ssh) = F.toList shs
  fzero = mkFrameShp sh
  insf s acc = mkFrameShp s `mappend` acc

mkFrameShp :: AdditiveGroup v => Shp p v v -> Frame v
mkFrameShp s = case s of
    C _ vd v -> mkFrame v (v ^+^ vd)
    R _ vd v -> mkFrame v (v ^+^ vd)

  
-- | Given :
--
-- * a starting frame (in the screen reference)
-- * a destination frame (in the SVG reference)
-- * a 'Shape' whose anchoring point is assumed to be bound by the starting frame
--
-- compose the affine transformations required to move the 'Shape' from starting to destination frame.
--
-- NB : this should be the /only/ function dedicated to transforming point coordinates
frameToFrameB :: (Bifunctor p, MatrixGroup (DiagMat2 a) v, Fractional a) =>
     Frame (V2 a) -> Frame (V2 a) -> p v (V2 a) -> p v (V2 a)
frameToFrameB from to = toFrameBimap to . second flipUD . fromFrameBimap from where
  flipUD (V2 vx vy) = mkV2 vx (1 - vy)  

fromFrameBimap :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
     Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
fromFrameBimap from = bimap f g
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)

toFrameBimap :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
     Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
toFrameBimap to = bimap f g
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto





-- | example smart constructors

mkC :: Num a => a -> ShapeCol p -> v -> Shp p (V2 a) v
mkC r col v = C col vd v where vd = r .* e1

mkR :: Num a => a -> a -> ShapeCol p -> v -> Shp p (V2 a) v
mkR w h col v = R col vd v where vd = fromCartesian w h

mkRBC w h col v = R col vs v where
  vs = fromCartesian (w/2) h
  v' = v ^-^ fromCartesian 0 (w/2)
  

mkReC :: Fractional a => a -> a -> ShapeCol p -> V2 a -> Shp p (V2 a) (V2 a)
mkReC w h col v = R col vs v' where
  vs = fromCartesian w h
  v' = v ^-^ (0.5 .* vs)  

mkSqrC :: Fractional a => a -> ShapeCol p -> V2 a -> Shp p (V2 a) (V2 a)
mkSqrC w col v = mkReC w w col v  

c4 = mkC 1 (shapeColNoBorder C.blue 0.6) (mkV2 30 15 )
c3 = mkC 1 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- r0 = mkR 5 5 (shapeColNoBorder C.red 1) (mkV2 20 20)
-- r1 = mkR 5 5 (shapeColNoBorder C.blue 1) (mkV2 0 0)

rectb w h x y = mkR w h (shapeColNoBorder C.red 1) (mkV2 x y)
r21 = rectb 5 5 0 0
r22 = rectb 5 10 10 0
r23 = rectb 5 2 20 0
r24 = rectb 5 15 30 0
-- -- shs = [r0, r1, c3]
-- -- shs = [r0, r1]

shs = [r21, r22, r23, r24, c3, c4]





render0 :: (Foldable t, Floating a, Real a, Functor t) =>
           Frame (V2 a)
        -> t (Shp a (V2 a) (V2 a))
        -> Svg
render0 to shs = renderShape `mapM_` reposition to shs


test0 =
  do
  let
    figdata = figureDataDefault
    to = frameFromFigData figdata
    (rout, rin) = rectsFigData figdata 
    svg_t = svgHeader' figdata $ do
      render0 to shs
      
      renderShape rout
      renderShape rin
  T.writeFile "examples/ex_dsl5.svg" $ T.pack $ renderSvg svg_t


-- | Rectangles based on the inner and outer frames of the drawable canvas
-- rectsFigData :: (Num p, Fractional a) => FigureData a -> (Sh p (V2 a), Sh p (V2 a))
rectsFigData fd = (rOut, rIn)
  where
    col = shapeColNoFill C.black 1 1
    frIn = frameFromFigData fd
    pc = midPoint (_fpmin frIn) (_fpmax frIn)
    rIn = mkReC (width frIn) (height frIn) col pc 
    rOut = mkReC (figWidth fd) (figHeight fd) col pc











{- |

YADG : Yet another DSL for graphics

Design :

* add dataset to Plot
* add Plot to WindowState (e.g. side by side plots, inset ... by specifying a RelativeFrame for it)
* compute all viewpanes (i.e. `to` frames)
* compute data transformations from viewpanes

-}

data PlotType = HeatMap | Scatter | TimeSeries 




-- | A `RelativeFrame` is given by two set of parameters:
--
-- 0 <= `rfX`, `rfY` <= 1 : normalized coordinates of the anchor point (top-left corner)
-- 0 <= `rfHeight`, `rfWidth` <= 1 : normalized width and height 
data RelativeFrame a = RelFrame
  { rfX :: a
  , rfY :: a
  , rfWidth :: a
  , rfHeight :: a
  } deriving (Eq, Show)

mkRelativeFrame :: (Ord a, Num a) => a -> a -> a -> a -> RelativeFrame a
mkRelativeFrame x y w h
  | all bounded01 [x,y,w,h] = RelFrame x y w h
  | otherwise = RelFrame 0 0 1 1

bounded01 :: (Ord a, Num a) => a -> Bool
bounded01 x = 0 <= x && x <= 1
    

-- data PlotAxis a = PlotAxis
--   { axType :: Axis
--   , axColour :: C.Colour Double
--   , axLabelFontSize :: Int
--   , axRangeMin :: a
--   , axRangeMax :: a
--   , axNTicks :: Int
--   , axTicks :: a -> T.Text  -- `a` is position parameter `0 <= lambda <= 1`
--   }

-- data Plot c a = Plot
--    { plRelativeFrame :: RelativeFrame a
--    , plAxisX :: PlotAxis a
--    , plAxisY :: PlotAxis a
--    , plContents :: c
--    }







-- data Window c a = W
--   { wWidth :: a
--   , wHeight :: a
--   , wState :: IM.IntMap (IM.IntMap (Plot c a))
--   }

-- data Layout c a s =
--   AddPlot (Window c a) (RelativeFrame a) (Window c a -> s)
--   deriving Functor


-- addPlot
--   :: Window c a -> RelativeFrame a -> Free (Layout c a) (Window c a)
-- addPlot w rf = liftF (AddPlot w rf id)


-- * Free


-- liftF :: Functor f => f r -> Free f r
-- liftF x = Free (fmap Pure x)

-- data Free f r = Free (f (Free f r)) | Pure r deriving Functor

-- instance Functor f => Applicative (Free f) where
--   pure = Pure

-- instance (Functor f) => Monad (Free f) where
--     return = pure
--     (Free x) >>= f = Free (fmap (>>= f) x)
--     (Pure r) >>= f = f r
