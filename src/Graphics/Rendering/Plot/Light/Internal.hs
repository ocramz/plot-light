{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveGeneric #-}
{-# language TypeFamilies, FlexibleContexts, ConstrainedClassMethods #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language TypeApplications #-}
{-# language GeneralizedNewtypeDeriving, DeriveTraversable #-}
module Graphics.Rendering.Plot.Light.Internal
  (
  -- * Frame
    Frame(..), mkFrame, unitFrame, mkFrameOrigin,
    -- frameToFrame,
    frameToFrameValue, frameFromPoints, frameFromFigData, xmin,xmax,ymin,ymax, width, height, frameToAffine,
    -- fromToStretchRatios, 
    -- * FigureData
    FigureData(..),
    -- figFWidth, figFHeight,
    figureDataDefault, withSvg
  --   -- * Point
  -- , Point(..), mkPoint,
  -- , origin
    -- * LabeledPoint
  , LabeledPoint(..), mkLabeledPoint, labelPoint, mapLabel, Axis(..),
    -- axes,
    meshGrid, subdivSegment,
    -- * SVG elements
    svgHeader, svgHeader',
    -- toPlot,
    -- ** Rectangle/square
    rect, rectCentered, rectCenteredMidpointBase, squareCentered,
    -- ** Circle
    circle,
    -- ** Lines
    line, line', tick, ticks,
    -- axis,
    -- ** Polylines
    polyline, filledPolyline,
    -- filledBand,
    strokeLineJoin, LineStroke_(..), StrokeLineJoin_(..),
    -- ** Text
    text, TextAnchor_(..), 
    -- ** Specialized plot elements
    pixel, pixel', plusGlyph, crossGlyph,
    -- candlestick,
    GlyphShape_,     
    -- ** Plot legend
    pickColour, colourBar, legendBar, LegendPosition_(..),

    -- * Geometry
    -- ** R^2 Vectors
    V2(..), mkV2, _vxy, origin, oneOne, e1, e2, norm2, normalize2,
    -- v2fromEndpoints, v2fromPoint, (-.),
    pointRange
    -- ** R^2 -> R^2 Matrices
  , Mat2(..), DiagMat2(..), diagMat2
    -- ** Typeclasses
  , AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..),
    -- movePoint, moveLabeledPointV2,
    -- moveLabeledPointBwFrames,
    translateSvg, scaleSvg, toBottomLeftSvgOrigin,
    -- toSvgFrame, toSvgFrameLP,
    toFloat, wholeDecimal
  -- * Colours
  , blendTwo, palette
    -- ** Col
  , (!#), Col(..), ShapeCol(..), mkCol, col50, col100, shapeColBoth
  , shapeColNoBorder, shapeColNoFill
    -- * General utility
    -- ** Function interpolation
  , interpolateBilinear)
  where

-- import Control.Arrow ((***), (&&&))
import Data.Semigroup -- (Semigroup(..))
import Data.Monoid

import qualified Data.Foldable as F (toList)

import Data.List
import Data.Functor.Identity
import Data.Bifunctor
-- import Control.Arrow (Arrow(..), (&&&), (***), first, second)
import qualified Data.List.NonEmpty as NE
import Control.Monad (forM, forM_)
import Control.Monad.State
-- import Data.Semigroup (Min(..), Max(..))
import Data.Scientific (Scientific, toRealFloat)

-- import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Text.Blaze.Internal (Attributable(..))
import Text.Blaze.Svg
import Text.Blaze.Svg11  ((!))
import qualified Text.Blaze.Svg11 as S hiding (style)
import qualified Text.Blaze.Svg11.Attributes as SA hiding (rotate)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import qualified Data.Histogram as H (Histogram(..), Bin(..), bins, asList)
import qualified Data.Histogram.Bin as H (BinD(..), binD, BinI(..), binI, binSize, UniformBin(..))
import qualified Data.Histogram.Fill as H (mkSimple, mkWeighted, fillBuilder, HBuilder(..))

import GHC.Real
import GHC.Generics hiding (from, to)
import Data.Fixed

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal.Utils



-- | Figure data
data FigureData a = FigureData {
  -- | Figure width
    figWidth :: a
    -- | Figure height
  , figHeight :: a
  -- | Left margin fraction (w.r.t figure width)
  , figLeftMFrac :: a
  -- | Right margin fraction (w.r.t figure width)  
  , figRightMFrac :: a
  -- | Top margin fraction (w.r.t figure height)
  , figTopMFrac :: a
  -- | Bottom margin fraction (w.r.t figure height)  
  , figBottomMFrac :: a
  -- -- | Axis stroke width
  -- , figAxisStrokeWidth :: a
  -- | Tick label font size
  , figLabelFontSize :: Int
                       } deriving (Eq, Show, Functor, Generic)

frameFromFigData :: Num a => FigureData a -> Frame (V2 a)
frameFromFigData fd = mkFrame oTo p2To where
    -- fontsize = figLabelFontSize fd
    wfig = figWidth fd
    hfig = figHeight fd
    (left, right) = (figLeftMFrac fd * wfig, figRightMFrac fd * wfig)
    (top, bot) = (figTopMFrac fd * hfig, figBottomMFrac fd * hfig)
    oTo = mkV2 left top
    p2To = mkV2 right bot



figureDataDefault :: Floating a => FigureData a
figureDataDefault = FigureData 400 300 0.1 0.9 0.1 0.9 10


bottomLeftOrigin :: Num a => FigureData a -> V2 a
bottomLeftOrigin fdat = mkV2 x y where
  x = figWidth fdat * figLeftMFrac fdat
  y = figHeight fdat * figBottomMFrac fdat




-- | Create the SVG header
svgHeader :: Real a =>
     a   -- ^ Image width (X axis)
  -> a   -- ^ Image height (Y axis)
  -> Svg -- ^ Image content
  -> Svg
svgHeader w h  =
  S.docTypeSvg
  ! SA.version "1.1"
  ! SA.width (vd w)
  ! SA.height (vd h)
  ! SA.viewbox (vds [xmin fd, ymin fd, xmax fd, ymax fd]) where
     fd = mkFrameOrigin w h

svgHeader' :: Real a => FigureData a -> Svg -> Svg
svgHeader' fdat = svgHeader (figWidth fdat) (figHeight fdat)

withSvg :: Real a => FigureData a -> (FigureData a -> Svg) -> Svg
withSvg figdat fn = do
  let svg_t = fn figdat
  svgHeader' figdat svg_t



-- | A Col is both a 'Colour' and an alpha (opacity) coefficient
data Col a = Col {
    cColour :: C.Colour Double  -- ^ Colour
  , cAlpha :: a                 -- ^ Opacity, [0 .. 1]
  } deriving (Eq, Show)

-- | 'Col' constructor
mkCol :: C.Colour Double -> a -> Col a
mkCol = Col

-- | Full opacity colour
col100 :: Num a => C.Colour Double -> Col a
col100 c = mkCol c 1

-- | Half opacity colour
col50 :: Fractional a => C.Colour Double -> Col a
col50 c = mkCol c 0.5

-- | A shape can either be only filled, or only contoured, or both
data ShapeCol a =
    NoBorderCol (Col a)  -- ^ Only fill colour
  | NoFillCol (Col a) a   -- ^ Only border colour + stroke width
  | BothCol (Col a) (Col a) a -- ^ Fill and border colours + stroke width
  deriving (Eq, Show)

-- | Construct a 'ShapeCol' for shapes that have no border stroke (i.e. have only the fill colour)
shapeColNoBorder :: C.Colour Double -> a -> ShapeCol a
shapeColNoBorder c a = NoBorderCol $ mkCol c a

-- | Construct a 'ShapeCol' for shapes that have no fill colour (i.e. have only the stroke colour)
shapeColNoFill ::
  C.Colour Double -- ^ Border colour
  -> a  -- ^ Opacity
  -> a  -- ^ Stroke width
  -> ShapeCol a
shapeColNoFill c a = NoFillCol $ mkCol c a 

-- | Construct a 'ShapeCol' for shapes that have both fill and stroke colour
shapeColBoth ::
     C.Colour Double  -- ^ Fill colour
  -> C.Colour Double  -- ^ Stroke colour
  -> a                -- ^ Opacity 
  -> a                -- ^ Stroke width
  -> ShapeCol a
shapeColBoth cs cf a = BothCol (mkCol cs a) (mkCol cf a)

-- | Set the fill and stroke colour and opacity attributes all at once (e.g. if the fill is set to invisible, the stroke must be visible somehow.
(!#) :: (Attributable h, Real a) => h -> ShapeCol a -> h
m !# col = case col of
  NoBorderCol (Col c a) ->
    m ! SA.fillOpacity (vd a) ! SA.fill (colourAttr c) ! SA.stroke none
  NoFillCol (Col c a) sw  ->
    m ! SA.strokeOpacity (vd a) ! SA.stroke (colourAttr c) ! SA.strokeWidth (vd sw) ! SA.fill none
  BothCol (Col cf af) (Col cb ab) sw ->
    m ! SA.fillOpacity (vd af) ! SA.fill (colourAttr cf) ! SA.strokeOpacity (vd ab) ! SA.stroke (colourAttr cb) ! SA.strokeWidth (vd sw)



none :: S.AttributeValue
none = S.toValue ("none" :: String)





-- | ===================
-- | Shape DSL 


-- | Screen reference system (origin is bottom-left screen corner)
data Screen = Screen deriving (Show)
-- | SVG reference system (origin is top-left screen corner)
data SVG = SVG deriving (Show)



-- | A V2 labeled by the reference frame
data V r a = V r (V2 a) deriving (Eq, Show)

mkVScreen :: V2 a -> V Screen a
mkVScreen = V Screen

mkVSvg :: V2 a -> V SVG a
mkVSvg = V SVG



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








-- * PLOTTING

-- | Line plot

-- mkPolyLinePlot :: Num a =>
--                   LineOptions p
--                -> StrokeLineJoin_
--                -> [a]
--                -> Shape p (V2 a)
-- mkPolyLinePlot lo slj dats = PolyLineSh lo slj ps where
--   n = length dats
--   ns = fromIntegral <$> [0 .. n-1]
--   ps = zipWith mkV2 ns dats

              


-- | Histogram

-- hist :: [(a, b)] -> Shape b (V2 a)  -- or something similar


-- | Returns the 'Frame' associated with a 'Histogram' along with the bins as 'LabeledPoint's (where the point coordinates lie on the X axis and the label contains the histogram count)
-- histGeometry :: (bv ~ H.BinValue bin, VU.Unbox bv, H.Bin bin, Ord bv, Num bv) =>
--                 H.Histogram bin bv
--              -> (Frame bv, [LabeledPoint bv bv])
-- histGeometry :: (bv ~ H.BinValue bin, VU.Unbox bv, H.Bin bin, Ord bv, Num bv) =>
--                 H.Histogram bin bv
--              -> Frame bv
histGeometry hist = frm where
  hl = H.asList hist
  -- hlps = map (\(x, bc) -> let p = Point x 0 in mkLabeledPoint p bc) hl
  (binCenters, binCounts) = unzip hl
  maxCount = maximum binCounts
  x1 = head binCenters
  x2 = last binCenters
  p1 = mkV2 x1 0
  p2 = mkV2 x2 maxCount
  frm = mkFrame p1 p2


-- | Normalized histogram counts (i.e. uniform density approximation) 
densityD :: (Fractional b, VU.Unbox b, Foldable v) =>
            Int
         -> v Double
         -> [(Double, b)]
densityD n = density . histo n

density :: (Fractional b, VU.Unbox b, H.Bin bin) =>
           H.Histogram bin b
        -> [(H.BinValue bin, b)] -- ^ (Bin centers, Normalized bin counts)
density hist = zip binCenters ((/ nelems) `map` binCounts) where
  (binCenters, binCounts) = unzip $ H.asList hist
  nelems = sum binCounts


-- | Uniform, un-weighted bins
histo :: (Foldable v, VU.Unbox a, Num a) =>
         Int     -- ^ Number of bins
      -> v Double -- ^ Data
      -> H.Histogram H.BinD a
histo n v = H.fillBuilder buildr v where
  mi = minimum v
  ma = maximum v + 1
  bins = H.binD mi n ma
  buildr = H.mkSimple bins

  




-- data Aligned a =
--     Centered (Shp a)
--   | BLCorner (Shp a)
--   | BSideC (Shp a)
--   deriving (Eq, Show, Functor)

-- data Shp a =
--     C a a
--   | R a a
--   deriving (Eq, Show, Functor)

-- mkRC :: a -> a -> Aligned a
-- mkRC v1 v2 = Centered (R v1 v2)


-- | -- 

-- data Shp a =
--     Ci a
--   | Re (Align a)
--   | Pt a 
--   deriving (Eq, Show)

-- data Align a =
--     Centered a
--   | BLCorner a
--   | BSideC a
--   deriving (Eq, Show)

-- renderShp sh = case sh of
--   Ci v -> undefined
--   Re al -> case al of
--     Centered v -> undefined



-- -- | -- 

-- -- | For some shapes we can specify the anchor point alignment. When the alignment is unspecified, the anchor point is taken to be the shape center by default.
-- data Shp p r a =
--     Ci r (ShapeCol p) a
--   | Re r r (ShapeCol p) (Align a)
--   -- | Gly GlyphShape_ a 
--   deriving (Eq, Show, Functor)

-- instance Bifunctor (Shp p) where
--   bimap f g sh = case sh of
--     Ci r col v -> Ci (f r) col (g v)
--     Re w h col v -> Re (f w) (f h) col (g <$> v)


-- data Align a =
--     Centered a
--   | BLCorner a
--   | BSideC a
--   deriving (Eq, Show, Functor)

-- -- -- | example smart constructor
-- -- mkCi :: Num a => p -> ShapeCol p -> a -> a -> Shp p (V2 a)
-- -- mkCi r col x y = Ci r col (fromCartesian x y)

-- -- renderShp sh = case sh of
-- --   Ci _ _ v -> undefined
-- --   Re _ _ _ al -> case al of
-- --     Centered v -> undefined




-- | -- 

-- | For some shapes we can specify the anchor point alignment. When the alignment is unspecified, the anchor point is taken to be the shape center by default.
data Shp p d a =
    Ci (ShapeCol p) d a
  | Re (ShapeCol p) d (Align a)
  -- | PyL [a]
  -- | Gly GlyphShape_ a 
  deriving (Eq, Show, Functor)

data Align a =
    Centered a
  | BLCorner a
  | BSideC a
  deriving (Eq, Show, Functor)

instance Bifunctor (Shp p) where
  bimap f g sh = case sh of
    Ci col wd w -> Ci col (f wd) (g w)
    Re col wd w -> Re col (f wd) (g <$> w)
    
linear :: (Bifunctor p, LinearMap m b) => m -> p b c -> p b c
linear m = first (\w -> m #> w)

linearInv :: (Bifunctor p, MatrixGroup m b) => m -> p b c -> p b c
linearInv m = first (\w -> m <\> w)

-- affine :: (Bifunctor p, LinearMap m b) => m -> b -> p b c -> p b c
-- affine m v = first (\w -> m #> (w ^+^ v))

-- affineInv :: (Bifunctor p, MatrixGroup m b) => m -> b -> p b c -> p b c
-- affineInv m v = first (\w -> m <\> (w ^-^ v))



-- | example smart constructors

mkCi :: Num a => a -> ShapeCol p -> v -> Shp p (V2 a) v
mkCi r col v = Ci col (fromCartesian r 0) v

mkReBLC :: Num a => a -> a -> ShapeCol p -> v -> Shp p (V2 a) v
mkReBLC w h col v = Re col vd (BLCorner v) where
  vd = fromCartesian w h 

-- | Compute center of shape
shpCenter sh = case sh of
  Ci _ _ v -> v
  Re _ vd al -> case al of
    Centered v -> v
    BLCorner v -> v ^+^ ((1/2) .* vd)

-- mkFrameShp :: AdditiveGroup v => Shp a v v -> Frame v
-- mkFrameShp sh = case sh of
--   Ci _ vd v -> mkFrame (v ^+^ vd) (v ^-^ vd)
--   Re _ vd al -> case al of
--     Centered v -> mkFrame (v ^+^ vd) (v ^-^ vd)
--     BLCorner v -> mkFrame v (v ^+^ vd)
--     -- BSideC v -> let (w, h) = _vxy vd

renderShp :: (Show a, RealFloat a) => Shp a (V2 a) (V2 a) -> Svg
renderShp sh = case sh of
  Ci col vd v -> circle r col v where
    r = norm2 vd
  Re col vd al -> case al of
    Centered v -> rectCentered w h col v where
      (w, h) = _vxy vd








-- | ==

-- | A NE is a NonEmpty list of things
newtype NE a = NE { unNE :: NE.NonEmpty a } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

singleton :: a -> NE a
singleton x = fromList x []

fromList :: a -> [a] -> NE a
fromList x xs = fromList' $ x : xs

fromList' :: [a] -> NE a
fromList' = NE . NE.fromList

toList :: NE a -> [a]
toList = NE.toList . unNE

neHead :: NE a -> a
neHead = NE.head . unNE

append :: NE a -> NE a -> NE a
append = liftNE2 (++)

liftNE2 :: ([a2] -> [a1] -> [a]) -> NE a2 -> NE a1 -> NE a
liftNE2 f u v = fromList' $ f (toList u) (toList v)







-- | =============
-- | A DSL for geometrical shapes
data Sh p a =
    Cir (ShapeCol p) a a
  | Rec (ShapeCol p) a a
  | RecC (ShapeCol p) a a
  | Sqr (ShapeCol p) a a
  | Line (LineOptions p) a a
  | PolyL (LineOptions p) StrokeLineJoin_ (NE a)
  deriving (Eq, Show, Functor)




-- Smart constructors

mkCir :: Num a => a -> ShapeCol p -> V2 a -> Sh p (V2 a)
mkCir r col vc = Cir col vc vcir where
  vcir = vc ^+^ (r .* e1)

mkRec :: Num a => a -> a -> ShapeCol p -> V2 a -> Sh p (V2 a)
mkRec w h col vc = Rec col vc v2 where
  v2 = vc ^+^ fromCartesian w h 

mkRecC :: Floating a => a -> a -> ShapeCol p -> V2 a -> Sh p (V2 a)
mkRecC w h col v = RecC col v1' v2' where
  vs = 0.5 .* fromCartesian w h 
  v1' = v ^-^ vs
  v2' = v ^+^ vs


mkSqr :: Num a => a -> ShapeCol p -> V2 a -> Sh p (V2 a)  
mkSqr r col vc = Sqr col vc v2 where
  v2 = vc ^+^ fromCartesian r r






-- c0 = mkCir 10 (shapeColNoBorder C.red 0.9) (mkV2 10 20)
-- c1 = mkCir 5 (shapeColNoBorder C.orange 0.9) (mkV2 5 15)
-- c2 = mkCir 15 (shapeColNoBorder C.blue 0.3) (mkV2 12 17)
c3 = mkCir 3 (shapeColNoBorder C.orange 0.5) (mkV2 0 0)

r0 = mkRec 10 10 (shapeColNoBorder C.red 1) (mkV2 10 20)
r1 = mkRec 10 10 (shapeColNoBorder C.blue 0.5) (mkV2 0 0)
-- r2 = mkRec 10 10 (shapeColNoBorder C.orange 0.7) (mkV2 5 10)

-- -- shs :: [Shape Double (Point Double)]
-- shs = [r0, r1, r2]
-- -- shs = [c0,c1,c2]
-- shs = [r0, r1, c3]
shs = [r0, r1]






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
  T.writeFile "examples/ex_dsl3.svg" $ T.pack $ renderSvg svg_t


-- | Rectangles based on the inner and outer frames of the drawable canvas
-- rectsFigData :: (Num p, Fractional a) => FigureData a -> (Sh p (V2 a), Sh p (V2 a))
rectsFigData fd = (rOut, rIn)
  where
    col = shapeColNoFill C.black 1 1
    frIn = frameFromFigData fd
    pc = midPoint (_fpmin frIn) (_fpmax frIn)
    rIn = mkRecC (width frIn) (height frIn) col pc 
    rOut = mkRecC (figWidth fd) (figHeight fd) col pc



render0 :: (Functor t, Foldable t, Show a, RealFloat a) =>
           Frame (V2 a)
        -> t (Sh a (V2 a))
        -> Svg
render0 to shs = renderShape `mapM_` wrapped to shs 

-- | NB : We must only render a 'Shape' that's in the SVG reference system
renderShape :: RealFloat a => Sh a (V2 a) -> Svg
renderShape sh = case sh of
  Cir col p1 p2 -> circle (norm2 $ p2 ^-^ p1) col p1
  Rec col p1 p2 -> rect w h' col p1' where
    h' = abs h
    (w, h) = _vxy (p2 ^-^ p1)
    p1' = p1  ^-^ fromCartesian 0 h'
  RecC col p1 p2 -> rect w h' col p1 where
    h' = abs h
    (w, h) = _vxy (p2 ^-^ p1)


-- |
-- 1) Computes the Frame that envelopes the input collection of points (i.e. the L1 convex hull)
-- 2) recomputes the point coordinates to fall within the destination frame in the SVG reference  
wrapped :: (Functor t, Foldable t, Fractional a, Ord a) =>
           Frame (V2 a)
        -> t (Sh p (V2 a))
        -> t (Sh p (V2 a))
wrapped to shs = convertShapeRef from to <$> shs where
  from = wrappingFrame shs
  

-- | Compute the 'Frame' that envelopes a 'Foldable' container (e.g. a list or vector) of 'Shape's.
--
-- The result can be used as the "from" Frame used to compute the Screen-SVG coordinate transform
-- wrappingFrame :: (Foldable t, Ord a, Monoid a) => t (Sh p a) -> Frame a
wrappingFrame shs = foldr fc mempty shs where
  fc acc b = mkShFrame acc `mappend` b

-- | Construct a Frame from a Sh
-- mkShFrame :: Ord a => Sh t a -> Frame a
mkShFrame :: (Fractional a, Ord a) => Sh t (V2 a) -> Frame (V2 a)
mkShFrame sh = case sh of
    Rec _ p1 p2 -> mkFrame p1 p2
    RecC _ p1 p2 -> mkFrame p1 p2
    Cir _ p1 p2 -> mkFrame p1 p2
    -- Sqr _ p1 p2 -> frameMidp p1 p2 -- mkFrame p1 p2
    PolyL _ _ ne -> frameFromPoints $ toList ne

frameMidp :: Fractional a => V2 a -> V2 a -> Frame (V2 a)
frameMidp p1 p2 = frameDirac $ midPoint p1 p2



-- | Given :
--
-- * a starting frame (in the screen reference)
-- * a destination frame (in the SVG reference)
-- * a 'Shape' whose anchoring point is assumed to be bound by the starting frame
--
-- compose the affine transformations required to move the 'Shape' from starting to destination frame.
--
-- NB : this should be the /only/ function dedicated to transforming point coordinates
convertShapeRef :: (Functor f, Fractional a) =>
                   Frame (V2 a)
                -> Frame (V2 a)
                -> f (V2 a)
                -> f (V2 a)
convertShapeRef from to sh = frameToFrame from to <$> sh


  
-- (<$$$>) :: (Functor f, Functor g, Functor h) => (x -> y) -> h (g (f x)) -> h (g (f y))
-- (<$$$>) = fmap . fmap . fmap

-- (<$$>) :: (Functor f, Functor g) => (x -> y) -> g (f x) -> g (f y)
-- (<$$>) = fmap . fmap
    



-- | A thing of type 'sh' in a 'Frame'
-- 
-- | the Frame parameter denotes the destination frame
--
-- NB : we can put more than one shape in a Frame
data Wrt r a sh = Wrt r (Frame a) sh deriving (Eq, Show, Functor)

-- -- instance Semigroup (Wrt r a sh) where
-- -- instance Monoid (Wrt r a sh) where 

-- wrtScreen :: Frame a -> sh -> Wrt Screen a sh
-- wrtScreen = Wrt Screen

wrtSvg :: Frame a -> sh -> Wrt SVG a sh
wrtSvg = Wrt SVG

-- screenToSvg :: Wrt Screen a sh -> Wrt SVG a sh
-- screenToSvg (Wrt Screen fr x) = Wrt SVG fr x                             








-- | ===================


-- | A rectangle, defined by its anchor point coordinates and side lengths
--
-- > > putStrLn $ renderSvg $ rect 50 60 (shapeColNoBorder C.blue 0.5) (Point 100 30)
-- > <rect x="100.0" y="30.0" width="50.0" height="60.0" fill-opacity="0.5" fill="#0000ff" stroke="none" />
rect :: Real a =>
        a          -- ^ Width
     -> a          -- ^ Height
     -> ShapeCol a -- ^ Colour and alpha information
     -> V2 a    -- ^ Corner point coordinates
     -> Svg
rect wid hei col p = S.rect ! SA.x (vd x0) ! SA.y (vd y0) ! SA.width (vd wid) ! SA.height (vd hei) !# col where
  (x0, y0) = _vxy p


-- | A rectangle, defined by its center coordinates and side lengths
--
-- > > putStrLn $ renderSvg $ rectCentered 15 30 (shapeColBoth C.blue C.red 1 5) (Point 20 30)
-- > <rect x="12.5" y="15.0" width="15.0" height="30.0" fill-opacity="1.0" fill="#0000ff" stroke-opacity="1.0" stroke="#ff0000" stroke-width="5.0" />
rectCentered :: (Show a, RealFrac a) =>
     a                       -- ^ Width
  -> a                       -- ^ Height
  -> ShapeCol a              -- ^ Colour and alpha information
  -> V2 a                 -- ^ Center coordinates     
  -> Svg
rectCentered  wid hei col p =
  rect wid hei col p' where
    (x0, y0) = _vxy p  
    p' = mkV2 x0c y0c
    x0c = x0 - (wid / 2)
    y0c = y0 - (hei / 2)   

-- | A rectangle, defined by the coordinates of the midpoint of its base
rectCenteredMidpointBase :: (Show a, RealFrac a) =>
     a                       -- ^ Width
  -> a                       -- ^ Height
  -> ShapeCol a              -- ^ Colour and alpha information
  -> V2 a                 -- ^ Base midpoint coordinates     
  -> Svg
rectCenteredMidpointBase wid hei col p =
  rect wid hei col p' where
    (x0, y0) = _vxy p  
    p' = mkV2 x0c y0
    x0c = x0 - (wid / 2)


-- | A square, defined by its center coordinates and side length
--
-- > > putStrLn $ renderSvg $ squareCentered 30 (shapeColBoth C.blue C.red 1 5) (Point 20 30)
-- > <rect x="5.0" y="15.0" width="30.0" height="30.0" fill-opacity="1.0" fill="#0000ff" stroke-opacity="1.0" stroke="#ff0000" stroke-width="5.0" />
squareCentered :: (Show a, RealFrac a) =>
                  a                          -- ^ Side length
               -> ShapeCol a              -- ^ Colour and alpha information
               -> V2 a                 -- ^ Center coordinates
               -> Svg
squareCentered w = rectCentered w w

lineColourDefault :: C.Colour Double
lineColourDefault = C.blue

lineStrokeTypeDefault :: LineStroke_ a
lineStrokeTypeDefault = Continuous

data LineOptions a = LineOptions {
    loStrokeWidth :: a            -- ^ Stroke width 
  , loStrokeType :: LineStroke_ a -- ^ Stroke type
  , loColour :: C.Colour Double   -- ^ Stroke colour
                                 } deriving (Eq, Show, Generic)

lineOptionsDefault :: Num a => LineOptions a
lineOptionsDefault = LineOptions 2 lineStrokeTypeDefault lineColourDefault

-- | Line options "picker". Creates an unbounded stream of LineOptions, may be useful when plotting multiple timeseries (essentially imitating the Matlab behaviour)
lineOptionCycle :: Fractional a => a -> [LineOptions a]
lineOptionCycle lw =
  let
    strTys =
      replicate 5 Continuous `mappend`
      replicate 5 (Dashed [0.2, 0.5]) `mappend`
      replicate 5 (Dashed [0.5, 0.2])       
    cols = [C.blue, C.green, C.red, C.black, C.purple]
    nc = length cols
  in
  LineOptions <$> repeat lw <*> strTys <*> cols



-- | Line segment between two `Point`s
-- 
-- > > putStrLn $ renderSvg $ line (mkV2 0 0) (mkV2 1 1) 0.1 Continuous C.blueviolet
-- > <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" />
--
-- > > putStrLn $ renderSvg (line (mkV2 0 0) (mkV2 1 1) 0.1 (Dashed [0.2, 0.3]) C.blueviolet)
-- > <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" stroke-dasharray="0.2, 0.3" />
line :: (Show a, RealFrac a) =>
     V2 a         -- ^ First point
  -> V2 a         -- ^ Second point
  -> a               -- ^ Stroke width
  -> LineStroke_ a   -- ^ Stroke type
  -> C.Colour Double -- ^ Stroke colour
  -> Svg
line p q sw lstr col =
  let
    (x1, y1) = _vxy p
    (x2, y2) = _vxy q    
    svg0 = S.line ! SA.x1 (vd x1) ! SA.y1 (vd y1) ! SA.x2 (vd x2)  ! SA.y2 (vd y2) ! SA.stroke (colourAttr col) ! SA.strokeWidth (vd sw)
  in case lstr of Continuous -> svg0
                  Dashed d -> svg0 ! strokeDashArray d

-- | Same as 'line' but using 'LineOptions'
line' :: (Show a, RealFrac a) => V2 a -> V2 a -> LineOptions a -> Svg
line' p1 p2 (LineOptions sw lstr col) = line p1 p2 sw lstr col


strokeDashArray :: Real a => [a] -> S.Attribute
strokeDashArray sz = SA.strokeDasharray (S.toValue str) where
  str = intercalate ", " $ map (show . real) sz

-- | Specify a continuous or dashed stroke
data LineStroke_ a = Continuous | Dashed [a] deriving (Eq, Show, Generic)




tick :: (Show a, RealFrac a) => Axis -> a -> a -> C.Colour Double -> V2 a -> Svg
tick ax len sw col p = line (mkV2 x1 y1) (mkV2 x2 y2) sw Continuous col where
  (x, y) = _vxy p
  lh = len / 2
  (x1, y1, x2, y2)
    | ax == Y = (x, y-lh, x, y+lh)
    | otherwise = (x-lh, y, x+lh, y)


plusGlyph, crossGlyph :: (Show a, RealFrac a) => a -> a -> C.Colour Double -> V2 a -> Svg
plusGlyph w sw k p = do
  line pl pr sw Continuous k
  line pt pb sw Continuous k
  where
    (x, y) = _vxy p    
    wh = w / 2
    pl = mkV2 (x-wh) y
    pr = mkV2 (x+wh) y
    pt = mkV2 x (y-wh)
    pb = mkV2 x (y+wh)

crossGlyph w sw k p = do
  line pa pb sw Continuous k
  line pc pd sw Continuous k
  where
    (x, y) = _vxy p        
    wh = 1.4142 * w
    pa = mkV2 (x+wh) (x+wh)
    pb = mkV2 (x-wh) (x-wh)
    pc = mkV2 (x+wh) (x-wh)
    pd = mkV2 (x-wh) (x+wh)


nsew :: Floating a => a -> V2 a -> [V2 a]
nsew alpha v =
  [ rotMtx a #> v | a <- [alpha, alpha + pi/2 , alpha + pi, alpha + 3/2 * pi]]


    


-- labeledTick :: (Show a, RealFrac a) =>
--                Axis
--             -> a                 -- ^ Length
--             -> a                 -- ^ Stroke width
--             -> C.Colour Double
--             -> Int               -- ^ Font size
--             -> a                 -- ^ Label angle
--             -> TextAnchor_     
--             -> (t -> T.Text)     -- ^ Label rendering function
--             -> V2 a              -- ^ Label shift 
--             -> LabeledPoint t a
--             -> Svg
labeledTick ax len sw col fontsize lrot tanchor flab vlab (LabeledPoint p label) = do
  tick ax len sw col p
  text lrot fontsize col tanchor (flab label) vlab p


-- | An array of axis-aligned identical segments (to be used as axis tickmarks), with centers given by the array of `Point`s
ticks :: (Foldable t, Show a, RealFrac a) =>
         Axis                -- ^ Axis 
      -> a                -- ^ Length         
      -> a                -- ^ Stroke width
      -> C.Colour Double  -- ^ Stroke colour
      -> t (V2 a)      -- ^ Center coordinates
      -> Svg
ticks ax len sw col ps = forM_ ps (tick ax len sw col)


-- labeledTicks :: (Foldable t, Show a, RealFrac a) =>
--                 Axis
--              -> a
--              -> a
--              -> C.Colour Double
--              -> Int
--              -> a
--              -> TextAnchor_
--              -> (t2 -> T.Text)
--              -> V2 a
--              -> t (LabeledPoint t2 a)
--              -> Svg
labeledTicks ax len sw col fontsize lrot tanchor flab vlab ps =
  forM_ ps (labeledTick ax len sw col fontsize lrot tanchor flab vlab)

-- | A plot axis with labeled tickmarks
--
-- > > putStrLn $ renderSvg $ axis (Point 0 50) X 200 2 C.red 0.05 Continuous 15 (-45) TAEnd T.pack (V2 (-10) 0) [LabeledPoint (Point 50 1) "bla", LabeledPoint (Point 60 1) "asdf"]
-- > <line x1="0.0" y1="50.0" x2="200.0" y2="50.0" stroke="#ff0000" stroke-width="2.0" /><line x1="50.0" y1="45.0" x2="50.0" y2="55.0" stroke="#ff0000" stroke-width="2.0" /><text x="-10.0" y="0.0" transform="translate(50.0 50.0)rotate(-45.0)" font-size="15" fill="#ff0000" text-anchor="end">bla</text><line x1="60.0" y1="45.0" x2="60.0" y2="55.0" stroke="#ff0000" stroke-width="2.0" /><text x="-10.0" y="0.0" transform="translate(60.0 50.0)rotate(-45.0)" font-size="15" fill="#ff0000" text-anchor="end">asdf</text>
-- axis :: (Functor t, Foldable t, Show a, RealFrac a) =>
--         Point a            -- ^ Origin coordinates
--      -> Axis            -- ^ Axis (i.e. either `X` or `Y`)
--      -> a               -- ^ Length of the axis
--      -> a               -- ^ Stroke width
--      -> C.Colour Double -- ^ Stroke colour
--      -> a               -- ^ The tick length is a fraction of the axis length
--      -> LineStroke_ a   -- ^ Stroke type
--      -> Int               -- ^ Label font size
--      -> a               -- ^ Label rotation angle
--      -> TextAnchor_     -- ^ How to anchor a text label to the axis
--      -> (l -> T.Text)   -- ^ How to render the tick label
--      -> V2 a            -- ^ Offset the label
--      -> t (LabeledPoint l a)     -- ^ Tick center coordinates
--      -> Svg
-- axis o@(Point ox oy) ax len sw col tickLenFrac ls fontsize lrot tanchor flab vlab ps = do
--       line o pend sw ls col
--       labeledTicks (otherAxis ax) (tickLenFrac * len) sw col fontsize lrot tanchor flab vlab (moveLabeledPoint f <$> ps)
--         where
--           pend | ax == X = Point (ox + len) oy
--                | otherwise = Point ox (oy + len)
--           f | ax == X = setPointY oy
--             | otherwise = setPointX ox

-- axis' axd@(AxisData n v o) (LineOptions sw ls col) = do
--   line o pend sw ls col
--   where
--     ps = mkAxisPoints axd
--     pend = last ps


-- -- | A pair of Cartesian axes
-- axes :: (Show a, RealFrac a) =>
--         FigureData a
--      -> Frame a
--      -> a
--      -> C.Colour Double
--      -> Int
--      -> Int
--      -> Svg
-- axes fdat (Frame (Point xmi ymi) (Point xma yma)) sw col nx ny = do
--   axis o X lenx sw col 0.01 Continuous fontsize (-45) TAEnd showlabf (V2 (-10) 0) plabx_
--   axis o Y (- leny) sw col 0.01 Continuous fontsize 0 TAEnd showlabf (V2 (-10) 0) plaby_
--   where
--     -- (Frame (Point xmi xymi) (Point xma yma)) = frameFromFigData fdat
--     o = bottomLeftOrigin fdat
--     pxend = movePoint (V2 lenx 0) o
--     pyend = movePoint (V2 0 (- leny)) o
--     plabx_ = zipWith LabeledPoint (pointRange nx o pxend) (take (nx+1) $ subdivSegment xmi xma $ fromIntegral nx)
--     plaby_ = zipWith LabeledPoint (pointRange ny o pyend) (take (ny+1) $ subdivSegment ymi yma $ fromIntegral ny)
--     fontsize = figLabelFontSize fdat
--     lenx = figFWidth fdat
--     leny = figFHeight fdat
--     showlabf x = T.pack $ show (fromRational x :: Fixed E2)
    














-- * text

-- | `text` renders text onto the SVG canvas
--
-- === Conventions
--
-- The `Point` argument `p` refers to the /lower-left/ corner of the text box.
--
-- The text box can be rotated by `rot` degrees around `p` and then anchored at either its beginning, middle or end to `p` with the `TextAnchor_` flag.
--
-- The user can supply an additional `V2` displacement which will be applied /after/ rotation and anchoring and refers to the rotated text box frame.
--
-- > > putStrLn $ renderSvg $ text (-45) C.green TAEnd "blah" (mkV2 (- 10) 0) (mkV2 250 0)
-- > <text x="-10.0" y="0.0" transform="translate(250.0 0.0)rotate(-45.0)" fill="#008000" text-anchor="end">blah</text>
text :: (Show a, Real a) =>
        a               -- ^ Rotation angle of the textbox
     -> Int             -- ^ Font size
     -> C.Colour Double -- ^ Font colour
     -> TextAnchor_     -- ^ How to anchor the text to the point
     -> T.Text          -- ^ Text 
     -> V2 a            -- ^ Displacement w.r.t. rotated textbox
     -> V2 a         -- ^ Initial position of the text box (i.e. before rotation and displacement)
     -> Svg
text rot fontsize col ta te (V2 vx vy) p = S.text_ (S.toMarkup te) ! SA.x (vd vx) ! SA.y (vd vy) ! SA.transform (S.translate (real x) (real y) `mappend` S.rotate (real rot)) ! SA.fontSize (vi fontsize) ! SA.fill (colourAttr col) ! textAnchor ta where
  (x, y) = _vxy p

-- | Specify at which end should the text be anchored to its current point
data TextAnchor_ = TAStart | TAMiddle | TAEnd deriving (Eq, Show)

textAnchor :: TextAnchor_ -> S.Attribute
textAnchor TAStart = SA.textAnchor (vs "start")
textAnchor TAMiddle = SA.textAnchor (vs "middle")
textAnchor TAEnd = SA.textAnchor (vs "end")




-- | A circle
--
-- > > putStrLn $ renderSvg $ circle 15 (shapeColBoth C.red C.blue 1 5) (Point 10 20)
-- > <circle cx="10.0" cy="20.0" r="15.0" fill-opacity="1.0" fill="#ff0000" stroke-opacity="1.0" stroke="#0000ff" stroke-width="5.0" />
circle :: (Real a1, Real a, Real p) =>
        a                       -- ^ Radius
     -> ShapeCol p 
     -> V2 a1                   -- ^ Center     
  -> Svg
circle r col p =
  S.circle ! SA.cx (vd x) ! SA.cy (vd y) ! SA.r (vd r) !# col where
  (x, y) = _vxy p





-- | Polyline (piecewise straight line)
-- 
-- > > putStrLn $ renderSvg (polyline [mkV2 100 50, mkV2 120 20, mkV2 230 50] 4 (Dashed [3, 5]) Round C.blueviolet)
-- > <polyline points="100.0,50.0 120.0,20.0 230.0,50.0" fill="none" stroke="#8a2be2" stroke-width="4.0" stroke-linejoin="round" stroke-dasharray="3.0, 5.0" />
polyline :: (Foldable t, Show a1, Show a, RealFrac a, RealFrac a1) =>
            a1              -- ^ Stroke width
         -> LineStroke_ a   -- ^ Stroke type 
         -> StrokeLineJoin_ -- ^ Stroke join type 
         -> C.Colour Double -- ^ Stroke colour
         -> t (V2 a)     -- ^ Data         
         -> Svg
polyline sw strTy slj col lis =
  let
    svg0 = S.polyline ! SA.points (S.toValue $ unwords $ map show $ F.toList lis) ! SA.fill none ! SA.stroke (colourAttr col ) ! SA.strokeWidth (vd sw) ! strokeLineJoin slj
  in case strTy of Continuous -> svg0
                   Dashed d -> svg0 ! strokeDashArray d


-- | Same as 'polyline' but using 'LineOptions'
polyline' :: (Foldable t, Show a, RealFrac a) => LineOptions a -> StrokeLineJoin_ -> t (V2 a) -> Svg
polyline' (LineOptions sw strTy col) slj lis = polyline sw strTy slj col lis




-- | A filled polyline
--
-- > > putStrLn $ renderSvg $ filledPolyline C.coral 0.3 [(mkV2 0 1), (mkV2 10 40), mkV2 34 50, Point 30 5]
-- > <polyline points="0,1 10,40 34,50 30,5" fill="#ff7f50" fill-opacity="0.3" />
filledPolyline :: (Foldable t, Show a, Real o) =>
                  C.Colour Double   -- ^ Fill colour
               -> o                 -- ^ Fill opacity
               -> t (V2 a)       -- ^ Contour point coordinates
               -> Svg
filledPolyline col opac lis = S.polyline ! SA.points (S.toValue $ unwords $ map show $ F.toList lis) ! SA.fill (colourAttr col) ! SA.fillOpacity (vd opac)


-- | A filled band of colour, given the coordinates of its center line
--
-- This element can be used to overlay uncertainty ranges (e.g. the first standard deviation) associated with a given data series.
-- -- filledBand :: (Foldable t, Real o, Show a) =>
-- --               C.Colour Double -- ^ Fill colour
-- --            -> o              -- ^ Fill opacity
-- --            -> (l -> a) -- ^ Band maximum value
-- --            -> (l -> a) -- ^ Band minimum value
-- --            -> t (LabeledPoint l a)    -- ^ Centerline points
-- --            -> Svg
-- filledBand col opac ftop fbot lis0 = filledPolyline col opac (lis1 <> lis2) where
--   lis = F.toList lis0
--   f1 lp = setPointY (ftop $ _lplabel lp) $ _lp lp
--   f2 lp = setPointY (fbot $ _lplabel lp) $ _lp lp
--   lis1 = f1  <$> lis
--   lis2 = f2  <$> reverse lis


-- | A `candlestick` glyph for time series plots. This is a type of box glyph, commonly used in plotting financial time series.
--
-- Some financial market quantities such as currency exchange rates are aggregated over some time period (e.g. a day) and summarized by various quantities, for example opening and closing rates, as well as maximum and minimum over the period.
--
-- By convention, the `candlestick` colour depends on the derivative sign of one such quantity (e.g. it is green if the market closes higher than it opened, and red otherwise).
-- candlestick
--   :: (Show a, RealFrac a) =>
--      (a -> a -> Bool)       -- ^ If True, fill the box with the first colour, otherwise with the second
--      -> (l -> a) -- ^ Box maximum value
--      -> (l -> a) -- ^ Box minimum value
--      -> (l -> a) -- ^ Line maximum value 
--      -> (l -> a) -- ^ Line minimum value
--      -> a                       -- ^ Box width
--      -> a                       -- ^ Stroke width
--      -> ShapeCol a              -- ^ First box colour
--      -> ShapeCol a              -- ^ Second box colour
--      -> C.Colour Double         -- ^ Line stroke colour
--      -> LabeledPoint l a        -- ^ Data point
--      -> Svg
-- candlestick fdec fboxmin fboxmax fmin fmax wid sw col1 col2 colstroke lp = do
--   line pmin pmax sw Continuous colstroke
--   rectCentered wid hei col p
--     where
--     p = _lp lp
--     lab = _lplabel lp
--     pmin = setPointY (fmin lab) p
--     pmax = setPointY (fmax lab) p
--     hei = abs $ fboxmax lab - fboxmin lab
--     col | fdec (fboxmax lab) (fboxmin lab) = col1
--         | otherwise = col2





-- | Specify the type of connection between line segments
data StrokeLineJoin_ = Miter | Round | Bevel | Inherit deriving (Eq, Show)

strokeLineJoin :: StrokeLineJoin_ -> S.Attribute
strokeLineJoin slj = SA.strokeLinejoin (S.toValue str) where
  str | slj == Miter = "miter" :: String
      | slj == Round = "round"
      | slj == Bevel = "bevel"
      | otherwise = "inherit"







-- | Move a Svg entity to a new position
translateSvg :: Show a => V2 a -> Svg -> Svg
translateSvg p svg = S.g ! SA.transform (S.translate x y) $ svg where
  (x, y) = _vxy p

-- | Scale a Svg entity
scaleSvg :: Real a => a -> a -> Svg -> Svg
scaleSvg sx sy svg = S.g ! SA.transform (S.scale (real sx) (real sy)) $ svg


-- | Flips and translates the SVG argument such that its axis origin lies at the bottom left corner defined in 'FigureData'.
toBottomLeftSvgOrigin :: Real a =>
                         FigureData a   
                      -> Svg
                      -> Svg
toBottomLeftSvgOrigin fdat svg = S.g ! SA.transform (S.translate (real 0) (real h) `mappend` S.scale (real 1) (real (- 1))) $ svg
  where
    h = _vy $ bottomLeftOrigin fdat





-- -- | Move point to the SVG frame of reference (for which the origing is a the top-left corner of the screen)
-- toSvgFrame ::
--   Fractional a =>
--      Frame a  -- ^ Initial frame
--   -> Frame a  -- ^ Final frame
--   -> Bool     -- ^ Flip L-R in [0,1] x [0,1]
--   -> Point a  -- ^ Point in the initial frame
--   -> Point a
-- toSvgFrame from to fliplr p = pointFromV2 v' where
--   v' = frameToFrame from to fliplr True (v2fromPoint p)


-- -- | Move LabeledPoint to the SVG frame of reference (uses `toSvgFrame` ) 
-- toSvgFrameLP ::
--   Fractional a => Frame a -> Frame a -> Bool -> LabeledPoint l a -> LabeledPoint l a
-- toSvgFrameLP from to fliplr (LabeledPoint p lab) = LabeledPoint (toSvgFrame from to fliplr p) lab




-- withToSvgFrame figdata dat = datf
--   where
--     from = frameFromPoints $ _lp <$> dat
--     to = frameFromFigData figdata
--     datf = toSvgFrameLP from to False -- data mapping function   






-- -- | A 'pixel' is a filled square shape used for populating 'heatmap' plots , coloured from a palette
-- pixel :: (Show a, RealFrac a) =>
--          [C.Colour Double]         -- ^ Palette
--       -> a                         -- ^ Width
--       -> a                         -- ^ Height
--       -> Scientific                -- ^ Function minimum
--       -> Scientific               -- ^ Function maximum
--       -> LabeledPoint Scientific a
--       -> Svg
pixel pal w h vmin vmax (LabeledPoint p l) = rectCentered w h col p where
  col = pickColour pal (toFloat vmin) (toFloat vmax) (toFloat l)

-- -- | A 'pixel'' is a filled square shape used for populating 'heatmap' plots , coloured from a palette
-- pixel'
--   :: (Show a, RealFrac a, RealFrac t) =>
--      [C.Colour Double]  -- ^ Palette
--   -> a                  -- ^ Width 
--   -> a -- ^ Height
--   -> t -- ^ Function minimum 
--   -> t -- ^ Function maximum
--   -> LabeledPoint t a
--   -> Svg
pixel' pal w h vmin vmax (LabeledPoint p l) = rectCentered w h col p where
  col = pickColour pal vmin vmax l
  

-- | Pick a colour from a list, assumed to be a palette mapped onto a compact numerical interval.
pickColour :: (RealFrac t, Num a) =>
        [C.Colour Double] -> t -> t -> t -> ShapeCol a
pickColour pal xmin xmax x = NoBorderCol $ Col (pal !! i) 1
  where
    i = floor (x01 * fromIntegral (nColors - 1))
    x01 = (x - xmin) / (xmax - xmin)
    nColors = length pal



data LegendPosition_ =
  TopLeft | TopRight | BottomLeft | BottomRight deriving (Eq, Show)

posCoeff :: Fractional a => LegendPosition_ -> (a, a)
posCoeff pos =
  case pos of
    TopLeft -> (0.1, 0.1)
    TopRight -> (0.83, 0.15)
    BottomLeft -> (0.1, 0.9)
    BottomRight -> (0.9, 0.9)


-- | A colour bar legend, to be used within `heatmap`-style plots.
colourBar
  :: (RealFrac t, RealFrac a, Show a, Enum t, Floating a) =>
     FigureData (Ratio Integer)  -- ^ Figure data
     -> [C.Colour Double]        -- ^ Palette
     -> a                        -- ^ Width
     -> t                        -- ^ Value range minimum
     -> t                        -- ^ Value range maximum
     -> Int                      -- ^ Number of distinct values
     -> LegendPosition_          -- ^ Legend position in the figure
     -> a                        -- ^ Colour bar length
     -> Svg
colourBar fdat pal w vmin vmax n legpos legh =
  legendBar (fromRational <$> fdat) w vmin vmax n legpos legh (colBarPx pal)


-- legendBar :: (Monad m, Enum t, Fractional t, Fractional a) =>
--              FigureData a
--           -> a
--           -> t
--           -> t
--           -> Int
--           -> LegendPosition_
--           -> a
--           -> (FigureData a -> a -> a -> t -> t -> LabeledPoint t a -> m b)
--           -> m ()
legendBar fdat w vmin vmax n legpos legh fun = do
  -- rect wrect hrect 1 (Just C.black) (Just C.white) prect
  forM_ lps (fun fdat w h vmin vmax) where
    wrect = 0.95 * (1 - figRightMFrac fdat) * figWidth fdat
    hrect = 1.5 * legh
    prect = (V2 (-0.5 * w) (-0.5 * w)) ^+^ p2
    (legx, legy) = posCoeff legpos
    legendX = figWidth fdat * legx 
    legendY = figHeight fdat * legy
    p1 = mkV2 legendX (legendY + legh)
    p2 = mkV2 legendX legendY
    lps = zipWith LabeledPoint (pointRange n p1 p2) v_
    h = legh / fromIntegral n
    v_ = take (n+1) [vmin, vmin + dv ..]
    dv = (vmax - vmin)/fromIntegral n


-- colBarPx
--   :: (Show a, RealFrac a, RealFrac t) =>
--      [C.Colour Double]       
--      -> FigureData a1
--      -> a
--      -> a
--      -> t
--      -> t
--      -> LabeledPoint t a
--      -> Svg
colBarPx pal fdat w h vmin vmax (LabeledPoint p val) = do
  text 0 (figLabelFontSize fdat) C.black TAStart (T.pack $ show (rr val :: Fixed E3)) (V2 (1.1*w) (0.5*h)) p
  rectCentered w h (pickColour pal vmin vmax val) p
  




    

  
-- * Helpers

-- | Render a Colour from `colour` into a `blaze` Attribute
colourAttr :: C.Colour Double -> S.AttributeValue
colourAttr = S.toValue . C.sRGB24show 



-- ** Conversion from primitive numerical types to AttributeValue

-- String

vs :: String -> S.AttributeValue
vs x = S.toValue (x :: String)

vi :: Int -> S.AttributeValue
vi = S.toValue


-- Double
vd0 :: Double -> S.AttributeValue
vd0 = S.toValue

vd :: Real a => a -> S.AttributeValue 
vd = vd0 . real

real :: (Real a, Fractional b) => a -> b
real = fromRational . toRational


vds :: Real a => [a] -> S.AttributeValue
vds = S.toValue . unwords . map (show . real)




