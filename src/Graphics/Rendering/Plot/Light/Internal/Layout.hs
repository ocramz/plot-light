{-# language TypeFamilies, DeriveFunctor, FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import Data.Bifunctor
import Data.Bifunctor.Pair
import qualified Data.Foldable as F (toList)
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Control.Monad (ap)

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Text.Blaze.Svg (Svg)
import Text.Blaze.Svg.Renderer.String (renderSvg)




-- | ========= 

-- data PlotTy a =
--     Scatter [a]
--   | PolyLine [a]
--   deriving (Eq, Show)






-- | =============
-- | A DSL for geometrical shapes
--
-- | A shape is :
--
-- * Anchored either at its center or not
-- * Has an anchor point (= position vector) and zero or more size vectors (e.g. a rectangle has only one size vector (i.e. is uniquely defined by its position vector and its size vector), a general N-polygon has N)



data Sh p vd v =
    Circle (ShapeCol p) vd v
  | Rect (ShapeCol p) vd (Align v) deriving (Eq, Show)

data Align v = BL v | BC v | C v deriving (Eq, Show, Functor)

instance Bifunctor (Sh p) where
  bimap f g sh = case sh of
    Circle k vd v -> Circle k (f vd) (g v)
    Rect k vd alv -> Rect k (f vd) (g <$> alv)

mkRectBL w h col v = Rect col vd (BL v) where vd = fromCartesian w h

mkRectC w h col v = Rect col vd (C v') where
  vd = fromCartesian w h
  v' = v ^-^ (vd ./ 2)

mkRectBC w h col v = Rect col vd (C v') where
  vd = fromCartesian w h
  v' = v ^-^ (projX vd ./ 2)

-- | retrieve the starting point.
--
-- NB inverts the function applied at construction time
getOriginalP sh = case sh of
  Circle _ _ v -> v
  Rect _ vd alv -> case alv of
    BL v -> v
    BC v -> v ^+^ (projX vd ./ 2)
    C v -> v ^+^ (vd ./ 2)



-- type Shape a = E (Sh a (V2 a) (V2 a))


-- * Constructors

-- -- -- mkRectBL :: Num a => a -> a -> ShapeCol a -> V2 a -> Shape a
-- mkRectBL w h col v = Rect col (BL vd v) where
--   vd = fromCartesian w h

-- mkRectC w h col v = Rect col (C vd v) where
--   vd = fromCartesian w h
--   v' = v ^-^ (vd ./ 2)

-- -- mkLin col p1 p2 = mkER $ Lin col p1 p2

-- getOriginalPoint sh = case sh of
--   Circle _ _ v -> v
--   Rect _ al -> case al of
--     BL vd v -> v ^+^ (0.5 .* vd)
--     BC vd v -> v ^+^ (0.5 .* projY vd)
--     C _ v  -> v 


-- -- 

-- -- | Render SVG markup from a Shape
-- shapeToSvg :: (Floating a, Show a, RealFrac a) => Sh a (V2 a) (V2 a) -> Svg 
-- shapeToSvg sh = case sh of
--   Circle col vd v -> circle r col v where r = norm2 vd
--   Rect col vd v -> rect w h col v where (w, h) = _vxy vd
--   Line opts v1 v2 -> line' v1 v2 opts
--   PLine opts slj vs -> polyline' opts slj vs




-- reposition to shs = reposition1 from to <$> shs where
--   from = wrappingFrame shs






-- | Given :
--
-- * a starting frame (in the screen reference)
-- * a destination frame (in the SVG reference)
-- * a 'Shape' whose anchoring point is assumed to be bound by the starting frame
--
-- compose the affine transformations required to move the 'Shape' from starting to destination frame.
--
-- NB : this should be the /only/ function dedicated to transforming point coordinates.

-- reposition1 :: Fractional a =>
--                Frame (V2 a)
--             -> Frame (V2 a)
--             -> E (Sh p (V2 a) (V2 a))
--             -> Sh p (V2 a) (V2 a)
-- reposition1 from to = eitherE (biasY . f2f) f2f where
--   f2f = frameToFrameB from to


-- -- | Modify the Y position component of a pair using both size and position parameters.
-- -- This only applies to non-centered shapes (such as the rectangle, which in SVG is anchored at its bottom-left corner)
biasY :: Num a => Sh p (V2 a) (V2 a) -> Sh p (V2 a) (V2 a)
biasY sh = case sh of
  Rect k vd alv -> Rect k vd (fbias vd alv)
  x -> x
  where
    fbias vd alv = case alv of
      BL v -> BL (v ^-^ fromCartesian 0 (_vy vd))
      BC v -> BC (v ^-^ fromCartesian 0 (_vy vd))
      c -> c


frameToFrameB :: (Bifunctor p, MatrixGroup (DiagMat2 a) b, Fractional a) =>
                  Frame (V2 a)
               -> Frame (V2 a)
               -> p b (V2 a)
               -> p b (V2 a)
frameToFrameB from to = toFrameB to . second flipUD . fromFrameB from
  where
    flipUD (V2 vx vy) = mkV2 vx (1 - vy)  
    
fromFrameB :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
               Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
fromFrameB from = bimap f g
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)        
    
toFrameB :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
             Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
toFrameB to = bimap f g
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto


-- mkHull :: (AdditiveGroup v, Functor f) => Pair (f v) v -> f v
-- mkHull (P vds v) = f <$> vds where
--   f vd = v ^+^ vd


-- | --


-- wrappingFrame :: (Foldable t, AdditiveGroup v, Ord v) => t (E (Sh p v v)) -> Frame v
-- wrappingFrame shs = foldr insf fzero ssh where
--   (sh:ssh) = F.toList shs
--   fzero = mkFrameSh sh
--   insf s acc = mkFrameSh s `mappend` acc

-- mkFrameSh :: (AdditiveGroup v, Ord v) => E (Sh p v v) -> Frame v
-- mkFrameSh ee = extractWith ff ee where
--   ff s = case s of
--     Circle _ vd v -> mkFrame v (v ^+^ vd)
--     Rect _ vd v -> mkFrame v (v ^+^ vd)
--     Line _ v1 v2 -> mkFrame v1 v2
--     PLine _ _ vs -> frameFromPoints vs


  















-- | example smart constructors

-- -- mkC :: Num a => a -> ShapeCol p -> v -> Shp p (V2 a) v
-- mkC r col v = Circle col vd v where vd = r .* e1

-- -- mkR :: Num a => a -> a -> ShapeCol p -> v -> Shp p (V2 a) v
-- mkR w h col v = RectBL col vd v where vd = fromCartesian w h



-- c4 = mkC 1 (shapeColNoBorder C.blue 0.6) (mkV2 30 15 )
-- c3 = mkC 1 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- -- r0 = mkR 5 5 (shapeColNoBorder C.red 1) (mkV2 20 20)
-- -- r1 = mkR 5 5 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- rectb w h x y = mkR w h (shapeColNoBorder C.red 1) (mkV2 x y)
-- r21 = rectb 5 5 0 0
-- r22 = rectb 5 10 10 0
-- r23 = rectb 5 2 20 0
-- r24 = rectb 5 15 30 0
-- -- -- shs = [r0, r1, c3]
-- -- -- shs = [r0, r1]

-- shs = [r21, r22, r23, r24, c3, c4]









-- -- render0 :: (Foldable t, Floating a, Real a, Functor t) =>
-- --            Frame (V2 a)
-- --         -> t (Shp a (V2 a) (V2 a))
-- --         -> Svg
-- render0 to shs = renderShape `mapM_` reposition to shs


-- test0 =
--   do
--   let
--     figdata = figureDataDefault
--     to = frameFromFigData figdata
--     (rout, rin) = rectsFigData figdata 
--     svg_t = svgHeader' figdata $ do
--       render0 to shs
--       renderShape rout
--       renderShape rin
--   T.writeFile "examples/ex_dsl5.svg" $ T.pack $ renderSvg svg_t


-- -- | Rectangles based on the inner and outer frames of the drawable canvas
-- -- rectsFigData :: (Num p, Fractional a) => FigureData a -> (Sh p (V2 a), Sh p (V2 a))
-- rectsFigData fd = (rOut, rIn)
--   where
--     col = shapeColNoFill C.black 1 1
--     frIn = frameFromFigData fd
--     pc = midPoint (_fpmin frIn) (_fpmax frIn)
--     rIn = mkReC (width frIn) (height frIn) col pc 
--     rOut = mkReC (figWidth fd) (figHeight fd) col pc





-- | Some data structs for plotting options

-- | Glyph shape for scatter plots
data GlyphShape_ =
  GlyphPlus | GlyphCross | GlyphCircle | GlyphSquare deriving (Eq, Show)


-- -- -- | Scatterplot glyph shapes
-- -- glyph :: (Show a, RealFrac a) =>
-- --          a               -- ^ Width
-- --       -> a               -- ^ Stroke width
-- --       -> GlyphShape_     -- ^ Glyph shape
-- --       -> C.Colour Double -- ^ Glyph colour
 -- --       -> a               -- ^ Opacity
-- --       -> Point a         -- ^ Position
-- --       -> Svg
-- glyph w sw sh col alpha p =
--   let cf = shapeColNoBorder col alpha
--   in 
--     case sh of
--       GlyphSquare -> squareCentered w cf p
--       GlyphCircle -> circle w cf p
--       GlyphCross -> crossGlyph w sw col p
--       GlyphPlus -> plusGlyph w sw col p

      

-- data PlotOptions p a =
--     LinePlot (LineOptions p) StrokeLineJoin_ a
--   | ScatterPlot Glyph_ (ShapeCol p) a
--   deriving (Eq, Show)


data AxisRange a = AxisRange {aoMin :: a, aoMax :: a}
  deriving (Eq, Show)

data Plot2d a =
  Plot2d (Frame a) (AxisRange a) (AxisRange a)
  deriving (Eq, Show)


data Plot =
    LinePlot
  | Histogram
  deriving (Eq, Show)









-- | =======
-- | HasFrame typeclass, a generic way to create a bounding Frame from something (e.g. a shape) that has a spatial extension

class HasFrame m where
  type FrameTy m :: *
  frame :: m -> Frame (FrameTy m)

instance HasFrame (V2 a) where
  type FrameTy (V2 a) = V2 a
  frame = frameDirac

instance HasFrame (Frame a) where
  type FrameTy (Frame a) = a
  frame = id

-- instance AdditiveGroup v => HasFrame (Shp p v v) where
--   type FrameTy (Shp p v v) = v
--   frame = mkFrameShp


-- -- a generic `wrappingFrame`
-- -- wrappingFrameG :: (Foldable t, HasFrame m, Ord (FrameTy m), Monoid (FrameTy m)) =>
-- --      t m -> Frame (FrameTy m)
-- wrappingFrameG shs = foldr insf fzero ssh where
--   (sh:ssh) = F.toList shs
--   fzero = frame sh
--   insf s acc = frame s `mappend` acc


-- | A thing of type 'sh' in a 'Frame'
-- 
-- | the Frame parameter denotes the destination frame
--
-- NB : we can put more than one shape in a Frame
data Wrt r a sh = Wrt r (Frame a) sh deriving (Eq, Show, Functor)

-- -- instance Semigroup (Wrt r a sh) where
-- -- instance Monoid (Wrt r a sh) where 



-- | Screen reference system (origin is bottom-left screen corner)
data Screen = Screen deriving (Show)
-- | SVG reference system (origin is top-left screen corner)
data SVG = SVG deriving (Show)


-- wrtScreen :: Frame a -> sh -> Wrt Screen a sh
-- wrtScreen = Wrt Screen

wrtSvg :: Frame a -> sh -> Wrt SVG a sh
wrtSvg = Wrt SVG

-- screenToSvg :: Wrt Screen a sh -> Wrt SVG a sh
-- screenToSvg (Wrt Screen fr x) = Wrt SVG fr x                             


-- | A V2 labeled by the reference frame
data V r a = V r (V2 a) deriving (Eq, Show)

mkVScreen :: V2 a -> V Screen a
mkVScreen = V Screen

mkVSvg :: V2 a -> V SVG a
mkVSvg = V SVG






-- | A composite collection type that carries stuff and a 'Frame' that delimits it

-- data Bag a x = Bag a (Frame x) deriving (Eq, Show)

-- instance (Monoid a, Monoid x, Ord x) => Monoid (Bag a x) where
--   mempty = Bag mempty mempty

-- (&) :: (Monoid a, Monoid x, Ord x) => Bag a x -> Bag a x -> Bag a x
-- (Bag f1 fr1) & (Bag f2 fr2) =
--   Bag (f1 `mappend` f2) (fr1 `mappend` fr2)










{- |

YADG : Yet another DSL for graphics

Design :

* add dataset to Plot
* add Plot to WindowState (e.g. side by side plots, inset ... by specifying a RelativeFrame for it)
* compute all viewpanes (i.e. `to` frames)
* compute data transformations from viewpanes

-}

-- data PlotType = HeatMap | Scatter | TimeSeries 




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
