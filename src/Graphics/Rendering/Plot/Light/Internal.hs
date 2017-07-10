{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light.Internal (FigureData(..), Point(..), LabeledPoint(..), Axis(..), mkFigureData, svgHeader, rectCentered, line, tick, ticks, axis, text, polyline, V2(..), Mat2(..), DiagMat2(..), diagMat2, AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), norm2, normalize2, mkV2fromEndpoints, v2fromPoint, origin, movePoint, moveLabeledPointV2, fromUnitSquare, toUnitSquare, e1, e2) where

import Data.Monoid ((<>))
import Control.Arrow ((&&&), (***))
import Control.Monad (forM, forM_)
import Data.Semigroup (Min(..), Max(..))
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

import Graphics.Rendering.Plot.Light.Internal.Types
import Graphics.Rendering.Plot.Light.Internal.Geometry



-- | Create a `FigureData` structure from the top-left corner point and its side lengths
mkFigureData :: Num a =>
          Point a      
       -> a
       -> a
       -> FigureData a
mkFigureData (Point xmi ymi) xlen ylen =
  FigData xlen ylen xmi (xmi + xlen) ymi (ymi + ylen)



-- | Create the SVG header from `FigureData`
svgHeader :: FigureData Int -> Svg -> Svg
svgHeader fd =
  S.docTypeSvg
  ! SA.version "1.1"
  ! SA.width (vi $ _width fd)
  ! SA.height (vi $ _height fd)
  ! SA.viewbox (vis [_xmin fd, _ymin fd, _xmax fd, _ymax fd])


-- | A filled rectangle, centered at (x0, y0)
rectCentered
  :: Point Double    -- ^ Center coordinates           
  -> Double          -- ^ Width
  -> Double          -- ^ Height
  -> C.Colour Double -- ^ Colour
  -> Svg
rectCentered (Point x0 y0) wid hei col = S.g ! SA.transform (S.translate x0c y0c) $ 
  S.rect ! SA.width (vd wid) ! SA.height (vd hei) ! SA.fill (colourAttr col) where
   x0c = x0 - (wid / 2)
   y0c = y0 - (hei / 2)   


-- | Line segment between two `Point`s
-- 
-- > > putStrLn $ renderSvg (line 0 0 1 1 0.1 C.blueviolet)
-- > <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" />
line :: Point Double -> Point Double -> Double -> C.Colour Double -> Svg
line (Point x1 y1) (Point x2 y2) sw col = S.line ! SA.x1 (vd x1) ! SA.y1 (vd y1) ! SA.x2 (vd x2)  ! SA.y2 (vd y2) ! SA.stroke (colourAttr col )! SA.strokeWidth (vd sw)

tick :: Axis -> Double -> Double -> C.Colour Double -> Point Double -> Svg
tick ax len sw col (Point x y) = line (Point x1 y1) (Point x2 y2) sw col where
  lh = len / 2
  (x1, y1, x2, y2)
    | ax == Y = (x, y-lh, x, y+lh)
    | otherwise = (x-lh, y, x+lh, y)

tickX, tickY ::
     Double          -- ^ Length
  -> Double          -- ^ Stroke width
  -> C.Colour Double -- ^ Stroke colour
  -> Point Double    -- ^ Center coordinates
  -> Svg
tickX = tick X
tickY = tick Y

-- | An array of axis-aligned identical segments (to be used as axis tickmarks), with centers given by the array of `Point`s
ticks :: Foldable t =>
               Axis                -- ^ Axis 
               -> Double           -- ^ Length         
               -> Double           -- ^ Stroke width
               -> C.Colour Double  -- ^ Stroke colour
               -> t (Point Double) -- ^ Center coordinates
               -> Svg
ticks ax len sw col ps = forM_ ps (tick ax len sw col)


-- | An axis with tickmarks
--
-- > > putStrLn $ renderSvg $ axis X 200 2 C.red 0.05 (Point 150 10) [Point 50 1, Point 60 1, Point 70 1]
-- > <line x1="50.0" y1="10.0" x2="250.0" y2="10.0" stroke="#ff0000" stroke-width="2.0" /><line x1="50.0" y1="5.0" x2="50.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" /><line x1="60.0" y1="5.0" x2="60.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" /><line x1="70.0" y1="5.0" x2="70.0" y2="15.0" stroke="#ff0000" stroke-width="2.0" />
axis :: (Functor t, Foldable t) =>
              Axis                  -- ^ Axis (i.e. either `X` or `Y`)
              -> Double             -- ^ Length
              -> Double             -- ^ Stroke width
              -> C.Colour Double    -- ^ Stroke colour
              -> Double             -- ^ Tick length fraction (w.r.t axis length)
              -> Point Double       -- ^ Axis center coordinate
              -> t (Point Double)   -- ^ Tick center coordinates
              -> Svg
axis ax len sw col tickLenFrac p@(Point x y) ps = do
  tick ax len sw col p
  ticks (otherAxis ax) (tickLenFrac * len) sw col (f <$> ps)
  where
    f | ax == X = setPointY y
      | otherwise = setPointX x



-- * text

-- | `text` renders text onto the SVG canvas. It is also possible to rotate and move the text, however the order of these modifiers matters.
-- 
-- NB1: `x` and `y` determine the position of the bottom-left corner of the text box. If a nonzero rotation is applied, the whole text box will move in a circle of radius || x^2 + y^2 ||
--
-- NB2: the `rotate` and `translate` attributes apply to the _center_ of the text box instead
--
-- > > putStrLn $ renderSvg $ text (-45) C.red "hullo!" (V2 (-30) 0) (Point 0 20)
-- > <text x="-30" y="0" transform="translate(0 20)rotate(-45)" fill="#ff0000">hullo!</text>
text :: (Show a, Show a1, S.ToValue a2) =>
              a1        -- ^ Rotation angle 
     -> C.Colour Double -- ^ Font colour
     -> T.Text          -- ^ Text 
     -> V2 a2           -- ^ Displacement
     -> Point a         -- ^ Initial center position of the text box
     -> Svg
text rot col te (V2 x y) (Point dx dy) = 
  S.text_ (S.toMarkup te) ! SA.x (S.toValue x) ! SA.y (S.toValue y) ! SA.transform (S.translate dx dy <> S.rotate rot) ! SA.fill (colourAttr col)



-- <polyline points="40 140 80 100 120 140" stroke="black" stroke-width="20" stroke-linecap="round" fill="none" stroke-linejoin="round"/>

-- <polyline points="20,20 40,25 60,40 80,120 120,140 200,180" style="fill:none;stroke:black;stroke-width:3" />

-- | Polyline (piecewise straight line)
-- 
-- > > putStrLn $ renderSvg (polyline [(1,1), (2,1), (2,2), (3,4)] 0.1 C.red)
-- > <polyline points="1,1 2,1 2,2 3,4" fill="none" stroke="#ff0000" stroke-width="0.1" />
polyline :: (Show a1, Show a) =>
            [(a1, a)]       -- ^ Data
         -> Double          -- ^ Stroke width
         -> C.Colour Double -- ^ Stroke colour
         -> Svg
polyline lis sw col = S.polyline ! SA.points (S.toValue $ unwords $ map showP2 lis) ! SA.fill none ! SA.stroke (colourAttr col )! SA.strokeWidth (vd sw) ! SA.strokeLinejoin (S.toValue ("round" :: String))

showP2 :: (Show a, Show a1) => (a1, a) -> String
showP2 (x, y) = show x ++ "," ++ show y 


none :: S.AttributeValue
none = S.toValue ("none" :: String)


  
-- * Helpers

-- | Render a Colour from `colour` into a `blaze` Attribute
colourAttr :: C.Colour Double -> S.AttributeValue
colourAttr = S.toValue . C.sRGB24show 


-- ** Conversion from primitive numerical types to AttributeValue
vi :: Int -> S.AttributeValue
vi = S.toValue

-- | For use e.g. in `viewbox`
vis :: [Int] -> S.AttributeValue
vis = S.toValue . unwords . map show

-- Double
vd :: Double -> S.AttributeValue
vd = S.toValue

vds :: [Double] -> S.AttributeValue
vds = S.toValue . unwords . map show

-- Float
vf :: Float -> S.AttributeValue
vf = S.toValue

vfs :: [Float] -> S.AttributeValue
vfs = S.toValue . unwords . map show


--

-- main :: IO ()
-- main = do
--   let a = renderSvg svgDoc
--   putStrLn a

-- svgDoc :: S.Svg
-- svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
--     S.g ! A.transform makeTransform $ do
--       -- S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
--       -- S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
--       S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
--       -- S.path ! A.d makePath

-- makePath :: S.AttributeValue
-- makePath = mkPath $ do
--   l 2 3
--   m 4 5

-- makeTransform :: S.AttributeValue
-- makeTransform = translate 1 1 -- rotate 50
