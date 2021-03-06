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
    line, line', tick, ticks, LineOptions(..),
    -- axis,
    -- ** Polylines
    polyline, polyline', filledPolyline,
    -- filledBand,
    strokeLineJoin, LineStroke_(..), StrokeLineJoin_(..),
    -- ** Text
    text, TextAnchor_(..), TextOptions(..),
    -- ** Specialized plot elements
    pixel, pixel', plusGlyph, crossGlyph,
    -- candlestick,
    -- GlyphShape_,    
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

import qualified Data.Foldable as F (toList, fold, foldMap)

import Data.List
import Data.Functor.Identity
import Data.Bifunctor
import Data.Bifunctor.Pair (Pair(..), Mix2(..))
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
import qualified Data.Colour.SRGB.Linear as C

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
  } deriving (Eq)
instance Show a => Show (Col a) where
  show (Col c _) = show c

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
  deriving (Eq)

instance Show a => Show (ShapeCol a) where
  show k = case k of
    NoBorderCol co -> show co
    NoFillCol co _ -> show co
    BothCol co1 co2 _ -> unwords [show co1, show co2]
    

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



data TextOptions a =
  TextOptions { toRot :: a   -- ^ Rotation angle
              , toFontSize :: Int -- ^ Font size
              , toColor :: C.Colour Double  -- ^ Font colour
              , toAnchor :: TextAnchor_  -- ^ Tech anchoring (start, middle, end)
              } deriving (Eq, Show)



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




