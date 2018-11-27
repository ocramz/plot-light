{-# language TypeFamilies, DeriveFunctor, FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import Data.Bifunctor
import Data.Bifunctor.Pair
import Data.Function ( (&) )
import qualified Data.Foldable as F (toList)
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Control.Arrow ((***), (&&&))
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



data Anchor v = AnchorBL v | AnchorBC v | AnchorC v deriving (Eq, Show, Functor)

data Sh p w v =
    Cir (ShapeCol p) w v
  | Rect (ShapeCol p) (Anchor v) w w v
  | Line (LineOptions p) v v
  | PLine (LineOptions p) [v]
  deriving (Eq, Show, Functor)

frameToFrameSh :: (Functor f, Fractional a) =>
                  Frame (V2 a) -> Frame (V2 a) -> f (V2 a) -> f (V2 a)
frameToFrameSh from to = toFrameSh to . flipSh . fromFrameSh from

fromFrameSh :: (Fractional a, Functor f) =>
               Frame (V2 a) -> f (V2 a) -> f (V2 a)
fromFrameSh from = fmap f where
  (mfrom, vfrom) = frameToAffine from
  f v = mfrom <\> (v ^-^ vfrom)

flipSh :: (Functor f, Num a) => f (V2 a) -> f (V2 a)
flipSh = fmap (\(V2 vx vy) -> V2 vx (1 - vy))

toFrameSh :: (Num a, Functor f) =>
             Frame (V2 a) -> f (V2 a) -> f (V2 a)
toFrameSh to = fmap f where
    (mto, vto) = frameToAffine to
    f v = (mto #> v) ^+^ vto



-- mkCir :: Num a => ShapeCol p -> a -> V2 a -> Sh p (V2 a)
-- mkCir col r pc = Sh fr (Cir col) where
--   fr = Frame pc (pc ^+^ fromCartesian 0 r)

-- mkRectC :: Fractional a => ShapeCol p -> a -> a -> V2 a -> Sh p (V2 a)
-- mkRectC col w h pc = Sh fr (Rect AnchorC col) where
--   fr = Frame pc (pc ^+^ fromCartesian (w / 2) (h / 2))

-- mkRectBC :: Fractional a => ShapeCol p -> a -> a -> V2 a -> Sh p (V2 a)
-- mkRectBC col w h pc = Sh fr (Rect AnchorBC col) where
--   fr = Frame pc (pc ^+^ fromCartesian (w / 2) h)

-- -- | ========= 

-- data Anchor = AnchorBL | AnchorBC | AnchorC deriving (Eq, Show)

-- data ShapeType p v =
--     Cir (ShapeCol p)
--   | Rect Anchor (ShapeCol p)
--   | Line (LineOptions p)
--   | PLine (LineOptions p) [v]
--   deriving (Eq, Show, Functor)

-- data Sh p v = Sh {
--     shFrame :: Frame v  -- ^ A geometrical shape is always defined by at least two points in the plane (i.e. a 'Frame')
--   , shType :: ShapeType p v
--   } deriving (Eq, Show, Functor)



-- -- | Transforms the Shape coordinates into the SVG reference frame
-- frameToFrameSh :: Fractional a =>
--                   Frame (V2 a) -> Frame (V2 a) -> Sh p (V2 a) -> Sh p (V2 a)
-- frameToFrameSh from to =
--   postprocessSh . toFrameSh to . midprocessSh . fromFrameSh from 

-- midprocessSh :: (Functor f, Num a) => f (V2 a) -> f (V2 a)
-- midprocessSh = fmap flipNormY where
--   flipNormY (V2 x y) = V2 x (1 - y)

-- postprocessSh :: Num a => Sh p (V2 a) -> Sh p (V2 a)
-- postprocessSh sh = case shType sh of
--   Rect anc _ -> sh{ shFrame = f (shFrame sh)}
--     where
--       f = postRectSh anc 
--   _ -> sh 

-- postRectSh :: Num a => Anchor -> Frame (V2 a) -> Frame (V2 a)
-- postRectSh anc fr@(Frame v1 v2)
--   | anc == AnchorBL = Frame (v1 ^-^ vh) (v2 ^+^ vh)
--   | anc == AnchorBL = Frame (v1 ^-^ vh ^-^ vw) (v2 ^+^ vh)
--   | anc == AnchorC  = Frame (v1 ^-^ vh ^-^ vw) (v2 ^+^ (2 .* vh))
--   where
--     (h, w) = (height fr, width fr)
--     vh = fromCartesian 0 h
--     vw = fromCartesian w 0    


-- fromFrameSh :: (Num a, MatrixGroup (DiagMat2 a) v, Functor f) =>
--                Frame (V2 a) -> f v -> f v
-- fromFrameSh from = fmap f where
--   (mfrom, _) = frameToAffine from
--   f v = mfrom <\> v

-- toFrameSh :: (Num a, LinearMap (DiagMat2 a) v, Functor f) =>
--              Frame (V2 a) -> f v -> f v
-- toFrameSh to = fmap f where
--     (mto, _) = frameToAffine to
--     f v = mto #> v












-- | =============
-- | A DSL for geometrical shapes
--
-- | A shape is :
--
-- * Anchored either at its center or not
-- * Has an anchor point (= position vector) and zero or more size vectors (e.g. a rectangle has only one size vector (i.e. is uniquely defined by its position vector and its size vector), a general N-polygon has N)



-- -- | Computing an enveloping Frame from a set of Shapes
-- --
-- -- NB : if all Shape anchor points are either X- or Y-collinear,
-- -- the frame will be degenerate (i.e. shrink into a line).
-- -- Therefore we need to catch those cases and symmetrically inflate the
-- -- frame around the degenerate axis.

-- -- mkHull :: (Foldable t, RealFloat a, Eps a) =>
-- --           t (Sh p (V2 a) (V2 a)) -> Frame (V2 a)
-- mkHull shs = foldr insf zh0 (tail zl) where
--   zl = F.toList shs
--   zh0 = mkFr $ head zl
--   mkFr s = frameDirac (getAnchor s)
--   insf sh fr
--     | qx && qy = growFrame (mkV2 (2 * dx) (2 * dy)) fr0
--     | qx = growFrameX (2 * dx) fr0
--     | qy = growFrameY (2 * dy) fr0
--     | otherwise = fr0
--     where
--       qx = nearZero (width fr0)
--       qy = nearZero (height fr0)
--       fr0 = mkFr sh <> fr
--       (dx, dy) = getDs sh
  

-- -- -- | get the (x, y) extent of a Shape
-- -- getDs :: RealFloat a => Sh p (V2 a) (V2 a) -> (a, a)
-- -- getDs sh = case sh of
-- --   Circle _ vd _ -> (r', r') where
-- --     r' = ceilD $ 0.5 * sqrt 2 * norm2 vd
-- --   Rect _ vd _ -> _vxy vd
-- --   Line _ v1 v2 -> (abs *** abs) . _vxy $ v1 ^-^ v2    

-- growFrameX, growFrameY :: Fractional a => a -> Frame (V2 a) -> Frame (V2 a)
-- growFrameX w = growFrame (fromCartesian (w / 2) 0)
-- growFrameY w = growFrame (fromCartesian 0 (w / 2))

-- -- | Grow a frame symmetrically along a vector
-- growFrame :: AdditiveGroup v => v -> Frame v -> Frame v
-- growFrame vd (Frame v1 v2) = mkFrame v1' v2' where
--   v1' = v1 ^-^ vd
--   v2' = v2 ^+^ vd
    

-- ceilD :: (RealFloat a, RealFloat b) => a -> b
-- ceilD = fromIntegral . ceiling


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
-- -- -- This only applies to non-centered shapes (such as the rectangle, which in SVG is anchored at its bottom-left corner)
-- biasY :: Num a => Sh p (V2 a) (V2 a) -> Sh p (V2 a) (V2 a)
-- biasY sh = case sh of
--   Rect k vd alv -> Rect k vd (fbias vd alv)
--   x -> x
--   where
--     fbias vd alv = case alv of
--       BL v -> BL (v ^-^ fromCartesian 0 (_vy vd))
--       BC v -> BC (v ^-^ fromCartesian 0 (_vy vd))
--       c -> c


frameToFrameB :: (Bifunctor p, MatrixGroup (DiagMat2 a) v, Fractional a) =>
                  Frame (V2 a)
               -> Frame (V2 a)
               -> p v (V2 a)
               -> p v (V2 a)
frameToFrameB from to = toFrameB to . second flipUD . fromFrameB from
  where
    flipUD (V2 vx vy) = mkV2 vx (1 - vy)  
    
fromFrameB :: (MatrixGroup (DiagMat2 a) v, Fractional a, Bifunctor p) =>
               Frame (V2 a) -> p v (V2 a) -> p v (V2 a)
fromFrameB from = bimap f g
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)        
    
toFrameB :: (Num a, LinearMap (DiagMat2 a) v, Bifunctor p) =>
             Frame (V2 a) -> p v (V2 a) -> p v (V2 a)
toFrameB to = bimap f g
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto





-- | --


  




-- | example smart constructors

-- c3, c4 :: Shape Double
-- c4 = mkCircle 1.0 (shapeColNoBorder C.blue 0.6) (mkV2 20 0 )
-- c3 = mkCircle 2.0 (shapeColNoBorder C.blue 1) (mkV2 10 0)

-- -- -- r0 = mkR 5 5 (shapeColNoBorder C.red 1) (mkV2 20 20)
-- -- -- r1 = mkR 5 5 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- -- rectb w h x y = mkR w h (shapeColNoBorder C.red 1) (mkV2 x y)
-- -- r21 = rectb 5 5 0 0
-- -- r22 = rectb 5 10 10 0
-- -- r23 = rectb 5 2 20 0
-- -- r24 = rectb 5 15 30 0
-- -- -- -- shs = [r0, r1, c3]
-- -- -- -- shs = [r0, r1]

-- -- shs = [r21, r22, r23, r24, c3, c4]









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
