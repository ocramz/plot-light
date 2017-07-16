{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light.Internal (FigureData(..), Frame(..), mkFrame, mkFrameOrigin, frameToFrame, frameFromPoints, width, height, Point(..), mkPoint, LabeledPoint(..), mkLabeledPoint, labelPoint,  Axis(..), svgHeader, rect, rectCentered, circle, line, tick, ticks, axis, toPlot, text, polyline, filledPolyline, filledBand, candlestick, strokeLineJoin, LineStroke_(..), StrokeLineJoin_(..), TextAnchor_(..), V2(..), Mat2(..), DiagMat2(..), diagMat2, AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..), norm2, normalize2, v2fromEndpoints, v2fromPoint, origin, (-.), pointRange, movePoint, moveLabeledPointV2, moveLabeledPointV2Frames, toSvgFrame, toSvgFrameLP, e1, e2) where

import Data.Monoid ((<>))
import qualified Data.Foldable as F (toList)
import Data.List
-- import Control.Arrow ((&&&), (***))
import Control.Monad (forM, forM_)
-- import Data.Semigroup (Min(..), Max(..))
import Data.Scientific (Scientific, toRealFloat)

-- import Data.Foldable
import qualified Data.Text as T
-- import qualified Data.Vector as V

import Text.Blaze.Svg
import Text.Blaze.Svg11  ((!))
import qualified Text.Blaze.Svg11 as S hiding (style)
import qualified Text.Blaze.Svg11.Attributes as SA hiding (rotate)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import GHC.Real

import Graphics.Rendering.Plot.Light.Internal.Geometry


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
  -- | Tick label font size
  , figLabelFontSize :: Int
                       } deriving (Eq, Show)



-- | Create the SVG header from a `Frame`
svgHeader :: Real a => Frame a -> Svg -> Svg
svgHeader fd =
  S.docTypeSvg
  ! SA.version "1.1"
  ! SA.width (vd $ width fd)
  ! SA.height (vd $ height fd)
  ! SA.viewbox (vds [xmin fd, ymin fd, xmax fd, ymax fd])


-- | A rectangle, defined by its anchor point coordinates and side lengths
--
-- > > putStrLn $ renderSvg $ rect (Point 100 200) 30 60 2 Nothing (Just C.aquamarine)
-- > <rect x="100.0" y="200.0" width="30.0" height="60.0" fill="#7fffd4" stroke="none" stroke-width="2.0" />
rect :: (Show a, RealFrac a) =>
     Point a                 -- ^ Center coordinates           
  -> a                       -- ^ Width
  -> a                       -- ^ Height
  -> a                       -- ^ Stroke width
  -> Maybe (C.Colour Double) -- ^ Stroke colour
  -> Maybe (C.Colour Double) -- ^ Fill colour  
  -> Svg
rect (Point x0 y0) wid hei sw scol fcol = S.rect ! SA.x (vd x0) ! SA.y (vd y0) ! SA.width (vd wid) ! SA.height (vd hei) ! colourFillOpt fcol ! colourStrokeOpt scol ! SA.strokeWidth (vd sw)


-- | A rectangle, defined by its center coordinates and side lengths
--
-- > > putStrLn $ renderSvg $ rectCentered (Point 20 30) 15 30 (Just C.blue) (Just C.red)
-- > <g transform="translate(12.5 15.0)"><rect width="15.0" height="30.0" fill="#ff0000" stroke="#0000ff" /></g>
rectCentered :: (Show a, RealFrac a) =>
     Point a                 -- ^ Center coordinates           
  -> a                       -- ^ Width
  -> a                       -- ^ Height
  -> a                       -- ^ Stroke width
  -> Maybe (C.Colour Double) -- ^ Stroke colour
  -> Maybe (C.Colour Double) -- ^ Fill colour  
  -> Svg
rectCentered (Point x0 y0) wid hei sw scol fcol = S.g ! SA.transform (S.translate x0c y0c) $ 
  S.rect ! SA.width (vd wid) ! SA.height (vd hei) ! colourFillOpt fcol ! colourStrokeOpt scol ! SA.strokeWidth (vd sw) where
   x0c = x0 - (wid / 2)
   y0c = y0 - (hei / 2)   


-- | Line segment between two `Point`s
-- 
-- > > putStrLn $ renderSvg $ line (Point 0 0) (Point 1 1) 0.1 Continuous C.blueviolet
-- > <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" />
--
-- > > putStrLn $ renderSvg (line (Point 0 0) (Point 1 1) 0.1 (Dashed [0.2, 0.3]) C.blueviolet)
-- > <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" stroke-dasharray="0.2, 0.3" />
line :: (Show a, RealFrac a) =>
     Point a         -- ^ First point
  -> Point a         -- ^ Second point
  -> a               -- ^ Stroke width
  -> LineStroke_ a   -- ^ Stroke type
  -> C.Colour Double -- ^ Stroke colour
  -> Svg
line (Point x1 y1) (Point x2 y2) sw Continuous col = S.line ! SA.x1 (vd x1) ! SA.y1 (vd y1) ! SA.x2 (vd x2)  ! SA.y2 (vd y2) ! SA.stroke (colourAttr col ) ! SA.strokeWidth (vd sw)
line (Point x1 y1) (Point x2 y2) sw (Dashed d) col = S.line ! SA.x1 (vd x1) ! SA.y1 (vd y1) ! SA.x2 (vd x2)  ! SA.y2 (vd y2) ! SA.stroke (colourAttr col ) ! SA.strokeWidth (vd sw) ! strokeDashArray d

strokeDashArray :: Real a => [a] -> S.Attribute
strokeDashArray sz = SA.strokeDasharray (S.toValue str) where
  str = intercalate ", " $ map (show . real) sz

-- | Specify a continuous or dashed stroke
data LineStroke_ a = Continuous | Dashed [a] deriving (Eq, Show)



tick :: (Show a, RealFrac a) => Axis -> a -> a -> C.Colour Double -> Point a -> Svg
tick ax len sw col (Point x y) = line (Point x1 y1) (Point x2 y2) sw Continuous col where
  lh = len / 2
  (x1, y1, x2, y2)
    | ax == Y = (x, y-lh, x, y+lh)
    | otherwise = (x-lh, y, x+lh, y)


labeledTick
  :: (Show a, RealFrac a) =>
     Axis
     -> a                 -- ^ Length
     -> a                 -- ^ Stroke width
     -> C.Colour Double
     -> Int               -- ^ Font size
     -> a                 -- ^ Label angle
     -> TextAnchor_     
     -> (t -> T.Text)     -- ^ Label rendering
     -> V2 a              -- ^ Label shift 
     -> LabeledPoint t a
     -> Svg
labeledTick ax len sw col fontsize lrot tanchor flab vlab (LabeledPoint p label) = do
  tick ax len sw col p
  text lrot fontsize col tanchor (flab label) vlab p


-- | An array of axis-aligned identical segments (to be used as axis tickmarks), with centers given by the array of `Point`s
ticks :: (Foldable t, Show a, RealFrac a) =>
               Axis                -- ^ Axis 
               -> a                -- ^ Length         
               -> a                -- ^ Stroke width
               -> C.Colour Double  -- ^ Stroke colour
               -> t (Point a)      -- ^ Center coordinates
               -> Svg
ticks ax len sw col ps = forM_ ps (tick ax len sw col)


labeledTicks ax len sw col fontsize lrot tanchor flab vlab ps =
  forM_ ps (labeledTick ax len sw col fontsize lrot tanchor flab vlab)

-- | A plot axis with labeled tickmarks
--
-- > > putStrLn $ renderSvg $ axis (Point 0 50) X 200 2 C.red 0.05 Continuous 15 (-45) TAEnd T.pack (V2 (-10) 0) [LabeledPoint (Point 50 1) "bla", LabeledPoint (Point 60 1) "asdf"]
-- > <line x1="0.0" y1="50.0" x2="200.0" y2="50.0" stroke="#ff0000" stroke-width="2.0" /><line x1="50.0" y1="45.0" x2="50.0" y2="55.0" stroke="#ff0000" stroke-width="2.0" /><text x="-10.0" y="0.0" transform="translate(50.0 50.0)rotate(-45.0)" font-size="15" fill="#ff0000" text-anchor="end">bla</text><line x1="60.0" y1="45.0" x2="60.0" y2="55.0" stroke="#ff0000" stroke-width="2.0" /><text x="-10.0" y="0.0" transform="translate(60.0 50.0)rotate(-45.0)" font-size="15" fill="#ff0000" text-anchor="end">asdf</text>
axis :: (Functor t, Foldable t, Show a, RealFrac a) =>
              Point a            -- ^ Origin coordinates
              -> Axis            -- ^ Axis (i.e. either `X` or `Y`)
              -> a               -- ^ Length of the axis
              -> a               -- ^ Stroke width
              -> C.Colour Double -- ^ Stroke colour
              -> a               -- ^ The tick length is a fraction of the axis length
              -> LineStroke_ a   -- ^ Stroke type
              -> Int               -- ^ Label font size
              -> a               -- ^ Label rotation angle
              -> TextAnchor_     -- ^ How to anchor a text label to the axis
              -> (l -> T.Text)   -- ^ How to render the tick label
              -> V2 a            -- ^ Offset the label
              -> t (LabeledPoint l a)     -- ^ Tick center coordinates
              -> Svg
axis o@(Point ox oy) ax len sw col tickLenFrac ls fontsize lrot tanchor flab vlab ps = do
      line o pend sw ls col
      labeledTicks (otherAxis ax) (tickLenFrac * len) sw col fontsize lrot tanchor flab vlab (moveLabeledPoint f <$> ps)
        where
          pend | ax == X = Point (ox + len) oy
               | otherwise = Point ox (oy + len)
          f | ax == X = setPointY oy
            | otherwise = setPointX ox



-- | `toPlot` performs a number of related operations:
--
-- * Maps the dataset to the figure frame
-- 
-- * Renders the X, Y axes
--
-- * Renders the transformed dataset onto the newly created plot canvas
toPlot
  :: (Functor t, Foldable t, Show a, RealFrac a) =>
     FigureData a     
     -> (l -> T.Text)  -- ^ X tick label
     -> (l -> T.Text)  -- ^ Y tick label
     -> a   -- ^ X label rotation angle
     -> a -- ^ Y label rotation angle
     -> a -- ^ Stroke width
     -> C.Colour Double -- ^ Stroke colour
     -> (t (LabeledPoint l a) -> t (LabeledPoint l a))  -- ^ X axis labels
     -> (t (LabeledPoint l a) -> t (LabeledPoint l a))  -- ^ Y axis labels
     -> (t (LabeledPoint l a) -> Svg)  -- ^ Data rendering function
     -> t (LabeledPoint l a) -- ^ Data
     -> Svg 
toPlot fd flabelx flabely rotx roty sw col1 tickXf tickYf plotf dat = do
  axis oSvg X (right - left) sw col1 0.05 Continuous fontsize rotx TAEnd flabelx (V2 (-10) 0) (tickXf dat')
  axis oSvg Y (top - bot) sw col1 0.05 Continuous fontsize roty TAEnd flabely (V2 (-10) 0) (tickYf dat')
  plotf dat'
  where
    fontsize = figLabelFontSize fd
    wfig = figWidth fd
    hfig = figHeight fd
    (left, right) = (figLeftMFrac fd * wfig, figRightMFrac fd * wfig)
    (top, bot) = (figTopMFrac fd * hfig, figBottomMFrac fd * hfig)
    oTo = Point left top
    p2To = Point right bot
    from = frameFromPoints $ _lp <$> dat
    to = mkFrame oTo p2To
    dat' = toSvgFrameLP from to False <$> dat
    oSvg = Point left bot





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
-- > > putStrLn $ renderSvg $ text (-45) C.green TAEnd "blah" (V2 (- 10) 0) (Point 250 0)
-- > <text x="-10.0" y="0.0" transform="translate(250.0 0.0)rotate(-45.0)" fill="#008000" text-anchor="end">blah</text>
text :: (Show a, Real a) =>
        a               -- ^ Rotation angle of the textbox
     -> Int             -- ^ Font size
     -> C.Colour Double -- ^ Font colour
     -> TextAnchor_     -- ^ How to anchor the text to the point
     -> T.Text          -- ^ Text 
     -> V2 a            -- ^ Displacement w.r.t. rotated textbox
     -> Point a         -- ^ Initial position of the text box (i.e. before rotation and displacement)
     -> Svg
text rot fontsize col ta te (V2 vx vy) (Point x y) = S.text_ (S.toMarkup te) ! SA.x (vd vx) ! SA.y (vd vy) ! SA.transform (S.translate (real x) (real y) <> S.rotate (real rot)) ! SA.fontSize (vi fontsize) ! SA.fill (colourAttr col) ! textAnchor ta

-- | Specify at which end should the text be anchored to its current point
data TextAnchor_ = TAStart | TAMiddle | TAEnd deriving (Eq, Show)

textAnchor :: TextAnchor_ -> S.Attribute
textAnchor TAStart = SA.textAnchor (vs "start")
textAnchor TAMiddle = SA.textAnchor (vs "middle")
textAnchor TAEnd = SA.textAnchor (vs "end")




-- | A circle
--
-- > > putStrLn $ renderSvg $ circle (Point 20 30) 15 (Just C.blue) (Just C.red)
-- > <circle cx="20.0" cy="30.0" r="15.0" fill="#ff0000" stroke="#0000ff" />
circle
  :: (Real a1, Real a) =>
     Point a1                   -- ^ Center
     -> a                       -- ^ Radius
     -> a                       -- ^ Stroke width
     -> Maybe (C.Colour Double) -- ^ Stroke colour
     -> Maybe (C.Colour Double) -- ^ Fill colour
  -> Svg
circle (Point x y) r sw scol fcol =
  S.circle ! SA.cx (vd x) ! SA.cy (vd y) ! SA.r (vd r) ! colourFillOpt fcol ! colourStrokeOpt scol ! SA.strokeWidth (vd sw) 





-- | Polyline (piecewise straight line)
-- 
-- > > putStrLn $ renderSvg (polyline [Point 100 50, Point 120 20, Point 230 50] 4 (Dashed [3, 5]) Round C.blueviolet)
-- > <polyline points="100.0,50.0 120.0,20.0 230.0,50.0" fill="none" stroke="#8a2be2" stroke-width="4.0" stroke-linejoin="round" stroke-dasharray="3.0, 5.0" />
polyline :: (Foldable t, Show a1, Show a, RealFrac a, RealFrac a1) =>
            t (Point a)     -- ^ Data
         -> a1              -- ^ Stroke width
         -> LineStroke_ a   -- ^ Stroke type 
         -> StrokeLineJoin_ -- ^ Stroke join type 
         -> C.Colour Double -- ^ Stroke colour
         -> Svg
polyline lis sw Continuous slj col = S.polyline ! SA.points (S.toValue $ unwords $ map show $ F.toList lis) ! SA.fill none ! SA.stroke (colourAttr col ) ! SA.strokeWidth (vd sw) ! strokeLineJoin slj
polyline lis sw (Dashed d) slj col = S.polyline ! SA.points (S.toValue $ unwords $ map show $ F.toList lis) ! SA.fill none ! SA.stroke (colourAttr col ) ! SA.strokeWidth (vd sw) ! strokeLineJoin slj ! strokeDashArray d

none :: S.AttributeValue
none = S.toValue ("none" :: String)

colourFillOpt :: Maybe (C.Colour Double) -> S.Attribute
colourFillOpt Nothing = SA.fill none
colourFillOpt (Just c) = SA.fill (colourAttr c)

colourStrokeOpt :: Maybe (C.Colour Double) -> S.Attribute
colourStrokeOpt Nothing = SA.stroke none
colourStrokeOpt (Just c) = SA.stroke (colourAttr c)


-- | A filled polyline
--
-- > > putStrLn $ renderSvg $ filledPolyline C.coral 0.3 [(Point 0 1), (Point 10 40), Point 34 50, Point 30 5]
-- > <polyline points="0,1 10,40 34,50 30,5" fill="#ff7f50" fill-opacity="0.3" />
filledPolyline :: (Foldable t, Show a, Real o) =>
                  C.Colour Double   -- ^ Fill colour
               -> o                 -- ^ Fill opacity
               -> t (Point a)       -- ^ Contour point coordinates
               -> Svg
filledPolyline col opac lis = S.polyline ! SA.points (S.toValue $ unwords $ map show $ F.toList lis) ! SA.fill (colourAttr col) ! SA.fillOpacity (vd opac)


-- | A filled band of colour, given the coordinates of its center line
--
-- This element can be used to overlay uncertainty ranges (e.g. the first standard deviation) associated with a given data series.
filledBand :: (Foldable t, Real o, Show a) =>
    C.Colour Double          -- ^ Fill colour
           -> o              -- ^ Fill opacity
           -> (LabeledPoint l a -> a) -- ^ Band maximum value
           -> (LabeledPoint l a -> a) -- ^ Band minimum value
           -> t (LabeledPoint l a)    -- ^ Centerline points
           -> Svg
filledBand col opac ftop fbot lis0 = filledPolyline col opac (lis1 <> lis2) where
  lis = F.toList lis0
  f1 lp = setPointY (ftop lp) $ _lp lp
  f2 lp = setPointY (fbot lp) $ _lp lp
  lis1 = f1  <$> lis
  lis2 = f2  <$> reverse lis


-- | A `candlestick` glyph for time series plots. This is a type of box glyph, commonly used in plotting financial time series.
--
-- Some financial market quantities such as currency exchange rates are aggregated over some time period (e.g. a day) and summarized by various quantities, for example opening and closing rates, as well as maximum and minimum over the period.
--
-- By convention, the `candlestick` colour depends on the derivative sign of one such quantity (e.g. it is green if the market closes higher than it opened, and red otherwise).
candlestick
  :: (Show a, RealFrac a) =>
     (LabeledPoint l a -> Bool)       -- ^ If True, fill the box with the first colour, otherwise with the second
     -> (LabeledPoint l a -> a) -- ^ Box maximum value
     -> (LabeledPoint l a -> a) -- ^ Box minimum value
     -> (LabeledPoint l a -> a) -- ^ Line maximum value 
     -> (LabeledPoint l a -> a) -- ^ Line minimum value
     -> a                       -- ^ Box width
     -> a                       -- ^ Stroke width
     -> C.Colour Double         -- ^ First box colour
     -> C.Colour Double         -- ^ Second box colour
     -> C.Colour Double         -- ^ Line stroke colour
     -> LabeledPoint l a        -- ^ Data point
     -> Svg
candlestick fdec fboxmin fboxmax fmin fmax wid sw col1 col2 colstroke lp = do
  line pmin pmax sw Continuous colstroke
  rectCentered p wid hei sw (Just colstroke) (Just col)
    where
    p = _lp lp
    pmin = setPointY (fmin lp) p
    pmax = setPointY (fmax lp) p
    hei = fboxmax lp - fboxmin lp
    col | fdec lp = col1
        | otherwise = col2





-- | Specify the type of connection between line segments
data StrokeLineJoin_ = Miter | Round | Bevel | Inherit deriving (Eq, Show)

strokeLineJoin :: StrokeLineJoin_ -> S.Attribute
strokeLineJoin slj = SA.strokeLinejoin (S.toValue str) where
  str | slj == Miter = "miter" :: String
      | slj == Round = "round"
      | slj == Bevel = "bevel"
      | otherwise = "inherit"


-- | Move point to the SVG frame of reference (for which the origing is a the top-left corner of the screen)
toSvgFrame ::
  Fractional a =>
     Frame a  -- ^ Initial frame
  -> Frame a  -- ^ Final frame
  -> Bool     -- ^ Flip L-R in [0,1] x [0,1]
  -> Point a  -- ^ Point in the initial frame
  -> Point a
toSvgFrame from to fliplr p = pointFromV2 v' where
  v' = frameToFrame from to fliplr True (v2fromPoint p)


-- | Move LabeledPoint to the SVG frame of reference (uses `toSvgFrame` ) 
toSvgFrameLP ::
  Fractional a => Frame a -> Frame a -> Bool -> LabeledPoint l a -> LabeledPoint l a
toSvgFrameLP from to fliplr (LabeledPoint p lab) = LabeledPoint (toSvgFrame from to fliplr p) lab







  
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







--       S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
--       -- S.path ! A.d makePath

-- makePath :: S.AttributeValue
-- makePath = mkPath $ do
--   l 2 3
--   m 4 5

-- makeTransform :: S.AttributeValue
-- makeTransform = translate 1 1 -- rotate 50
