{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light.Internal (Frame(..), Point(..), LabeledPoint(..), Axis(..), svgHeader, rectCentered, circle, line, tick, ticks, axis, text, polyline, strokeLineJoin, LineStroke_(..), StrokeLineJoin_(..), TextAnchor_(..), V2(..), Mat2(..), DiagMat2(..), diagMat2, AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..), norm2, normalize2, v2fromEndpoints, v2fromPoint, origin, movePoint, moveLabeledPointV2, fromUnitSquare, toUnitSquare, e1, e2) where

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



-- | Create the SVG header from a `Frame`
svgHeader :: Real a => Frame a -> Svg -> Svg
svgHeader fd =
  S.docTypeSvg
  ! SA.version "1.1"
  ! SA.width (vd $ width fd)
  ! SA.height (vd $ height fd)
  ! SA.viewbox (vds [xmin fd, ymin fd, xmax fd, ymax fd])



-- | A rectangle, defined by its center coordinates and side lengths
--
-- > > putStrLn $ renderSvg $ rectCentered (Point 20 30) 15 30 (Just C.blue) (Just C.red)
-- > <g transform="translate(12.5 15.0)"><rect width="15.0" height="30.0" fill="#ff0000" stroke="#0000ff" /></g>
rectCentered :: (Show a, RealFrac a) =>
     Point a                 -- ^ Center coordinates           
  -> a                       -- ^ Width
  -> a                       -- ^ Height
  -> Maybe (C.Colour Double) -- ^ Stroke colour
  -> Maybe (C.Colour Double) -- ^ Fill colour  
  -> Svg
rectCentered (Point x0 y0) wid hei scol fcol = S.g ! SA.transform (S.translate x0c y0c) $ 
  S.rect ! SA.width (vd wid) ! SA.height (vd hei) ! colourFillOpt fcol ! colourStrokeOpt scol where
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



tick :: (Show a, RealFrac a) => Axis -> a -> a -> C.Colour Double -> LineStroke_ a -> Point a -> Svg
tick ax len sw col ls (Point x y) = line (Point x1 y1) (Point x2 y2) sw ls col where
  lh = len / 2
  (x1, y1, x2, y2)
    | ax == Y = (x, y-lh, x, y+lh)
    | otherwise = (x-lh, y, x+lh, y)

tickX, tickY :: (Show a, RealFrac a) =>
     a               -- ^ Length
  -> a               -- ^ Stroke width
  -> C.Colour Double -- ^ Stroke colour
  -> LineStroke_ a
  -> Point a         -- ^ Center coordinates
  -> Svg
tickX = tick X
tickY = tick Y

-- | An array of axis-aligned identical segments (to be used as axis tickmarks), with centers given by the array of `Point`s
ticks :: (Foldable t, Show a, RealFrac a) =>
               Axis                -- ^ Axis 
               -> a                -- ^ Length         
               -> a                -- ^ Stroke width
               -> C.Colour Double  -- ^ Stroke colour
               -> LineStroke_ a
               -> t (Point a)      -- ^ Center coordinates
               -> Svg
ticks ax len sw col ls ps = forM_ ps (tick ax len sw col ls)


-- | An axis with tickmarks
--
-- > > putStrLn $ renderSvg $ axis X 200 2 C.red 0.05 (Point 150 10) Continuous [Point 50 1, Point 60 1, Point 70 1]
-- > <line x1="50.0" y1="10.0" x2="250.0" y2="10.0" stroke="#ff0000" stroke-width="2.0" /><line x1="50.0" y1="5.0" x2="50.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" /><line x1="60.0" y1="5.0" x2="60.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" /><line x1="70.0" y1="5.0" x2="70.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" />
axis :: (Functor t, Foldable t, Show a, RealFrac a) =>
              Point a            -- ^ Origin coordinates
              -> Axis            -- ^ Axis (i.e. either `X` or `Y`)
              -> a               -- ^ Length
              -> a               -- ^ Stroke width
              -> C.Colour Double -- ^ Stroke colour
              -> a               -- ^ Tick length fraction (w.r.t axis length)
              -> LineStroke_ a
              -> t (Point a)     -- ^ Tick center coordinates
              -> Svg
axis o@(Point ox oy) ax len sw col tickLenFrac ls ps = do
      line o pend sw ls col
      ticks (otherAxis ax) (tickLenFrac * len) sw col ls (f <$> ps)
        where
          pend | ax == X = Point (ox + len) oy
               | otherwise = Point ox (oy + len)
          f | ax == X = setPointY oy
            | otherwise = setPointX ox


-- * text

-- | `text` renders text onto the SVG canvas
--
-- == Conventions
--
-- The `Point` argument `p` refers to the /lower-left/ corner of the text box.
--
-- After the text is rendered, its text box can be rotated by `rot` degrees around `p` and then optionally anchored.
--
-- The user can supply an additional `V2` displacement which will be applied /after/ rotation and anchoring and refers to the rotated text box frame.
--
-- > > putStrLn $ renderSvg $ text (-45) C.green TAEnd "blah" (V2 (- 10) 0) (Point 250 0)
-- > <text x="-10.0" y="0.0" transform="translate(250.0 0.0)rotate(-45.0)" fill="#008000" text-anchor="end">blah</text>
text :: (Show a, Real a) =>
        a               -- ^ Rotation angle of the frame 
     -> C.Colour Double -- ^ Font colour
     -> TextAnchor_
     -> T.Text          -- ^ Text 
     -> V2 a            -- ^ Displacement w.r.t. rotated frame
     -> Point a         -- ^ Reference frame origin of the text box
     -> Svg
text rot col ta te (V2 vx vy) (Point x y) = S.text_ (S.toMarkup te) ! SA.x (vd vx) ! SA.y (vd vy) ! SA.transform (S.translate (real x) (real y) <> S.rotate (real rot)) ! SA.fill (colourAttr col) ! textAnchor ta

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
     -> Maybe (C.Colour Double) -- ^ Stroke colour
     -> Maybe (C.Colour Double) -- ^ Fill colour
  -> Svg
circle (Point x y) r scol fcol =
  S.circle ! SA.cx (vd x) ! SA.cy (vd y) ! SA.r (vd r) ! colourFillOpt fcol ! colourStrokeOpt scol





-- | Polyline (piecewise straight line)
-- 
-- > > putStrLn $ renderSvg (polyline [Point 100 50, Point 120 20, Point 230 50] 4 (Dashed [3, 5]) Round C.blueviolet)
-- > <polyline points="100.0,50.0 120.0,20.0 230.0,50.0" fill="none" stroke="#8a2be2" stroke-width="4.0" stroke-linejoin="round" stroke-dasharray="3.0, 5.0" />
polyline :: (Foldable t, Show a1, Show a, RealFrac a, RealFrac a1) =>
            t (Point a)     -- ^ Data
         -> a1              -- ^ Stroke width
         -> LineStroke_ a
         -> StrokeLineJoin_  
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


-- | Specify the type of connection between line segments
data StrokeLineJoin_ = Miter | Round | Bevel | Inherit deriving (Eq, Show)

strokeLineJoin :: StrokeLineJoin_ -> S.Attribute
strokeLineJoin slj = SA.strokeLinejoin (S.toValue str) where
  str | slj == Miter = "miter" :: String
      | slj == Round = "round"
      | slj == Bevel = "bevel"
      | otherwise = "inherit"







  
-- * Helpers

-- | Render a Colour from `colour` into a `blaze` Attribute
colourAttr :: C.Colour Double -> S.AttributeValue
colourAttr = S.toValue . C.sRGB24show 


-- ** Conversion from primitive numerical types to AttributeValue

-- String

vs :: String -> S.AttributeValue
vs x = S.toValue (x :: String)


-- Double
vd0 :: Double -> S.AttributeValue
vd0 = S.toValue

vd :: Real a => a -> S.AttributeValue 
vd = vd0 . real

real :: (Real a, Fractional b) => a -> b
real = fromRational . toRational


vds :: Real a => [a] -> S.AttributeValue
vds = S.toValue . unwords . map (show . real)





-- -- Float
-- vf :: Float -> S.AttributeValue
-- vf = S.toValue

-- vfs :: [Float] -> S.AttributeValue
-- vfs = S.toValue . unwords . map show



--       S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
--       -- S.path ! A.d makePath

-- makePath :: S.AttributeValue
-- makePath = mkPath $ do
--   l 2 3
--   m 4 5

-- makeTransform :: S.AttributeValue
-- makeTransform = translate 1 1 -- rotate 50
