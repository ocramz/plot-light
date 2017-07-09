{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light.Internal where

import Control.Arrow ((&&&), (***))
import Data.Semigroup (Min(..), Max(..))
import Data.Scientific (Scientific, toRealFloat)

-- import Data.Foldable
import qualified Data.Text as T
-- import qualified Data.Vector as V

import Text.Blaze.Svg
import Text.Blaze.Svg11  ((!))
import qualified Text.Blaze.Svg11 as S hiding (style)
import qualified Text.Blaze.Svg11.Attributes as S
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import GHC.Real

import Graphics.Rendering.Plot.Light.Internal.Types


-- axis f1 figd = undefined
--   where
--     d = f1 <$> _figData figd -- pick out data
--     numd = length d
--     mm = maximum d
--     m = minimum d

asf f r1 r2 = (m, me, mm) where
  r1' = f r1
  r2' = f r2
  m = min r1' r2'
  mm = max r1' r2'
  me = (r1' + r2')/2




-- | Remap the figure data to fit within [0, xPlot], [0, yPlot]
-- mkFigureData :: (Fractional a, Ord a) => a -> a -> [(a, a)] -> ([(a, a)], FigureData a)
mkFigureData xPlot yPlot fy dat = (dat', FigData xPlot yPlot 0 xPlot 0 yPlot) where
  (x_, y0_) = unzip dat
  y_ = map fy y0_
  (xmax, xmin) = (maximum &&& minimum) x_
  xd = xmax - xmin
  (ymax, ymin) = (maximum &&& minimum) y_
  yd = ymax - ymin
  remapX = affine 0 xPlot xmin xmax
  remapY = affine 0 yPlot ymin ymax
  flipY y = yPlot - y
  dat' = zip (map remapX x_) (map (flipY . remapY) y_)  


-- | Header for a Figure
figure :: FigureData Double -> Svg -> Svg
figure fd =
  S.docTypeSvg
  ! S.version "1.1"
  ! S.width (vd $ _width fd)
  ! S.height (vd $ _height fd)
  ! S.viewbox (vds [_xmin fd, _ymin fd, _xmax fd, _ymax fd])


-- | A filled rectangle, centered at (x0, y0)
rectCentered
  :: Double -> Double -> Double -> Double -> C.Colour Double -> Svg
rectCentered x0 y0 wid hei col = S.g ! S.transform (S.translate x0c y0c) $ 
  S.rect ! S.width (vd wid) ! S.height (vd hei) ! S.fill (colourAttr col) where
   x0c = x0 - (wid / 2)
   y0c = y0 - (hei / 2)   

-- centeredAt x0 y0 = S.g ! A.transform (translate x0 y0) 

-- | Line segment
--
-- e.g. 
-- > putStrLn $ renderSvg (line 0 0 1 1 0.1 C.blueviolet)
-- 
-- <line x1="0.0" y1="0.0" x2="1.0" y2="1.0" stroke="#8a2be2" stroke-width="0.1" />
line :: Double -> Double -> Double -> Double -> Double -> C.Colour Double -> Svg
line x1 y1 x2 y2 sw col = S.line ! S.x1 (vd x1) ! S.y1 (vd y1) ! S.x2 (vd x2)  ! S.y2 (vd y2) ! S.stroke (colourAttr col )! S.strokeWidth (vd sw)




-- <polyline points="40 140 80 100 120 140" stroke="black" stroke-width="20" stroke-linecap="round" fill="none" stroke-linejoin="round"/>

-- <polyline points="20,20 40,25 60,40 80,120 120,140 200,180" style="fill:none;stroke:black;stroke-width:3" />

-- | Polyline (piecewise straight line)
-- e.g.
-- > putStrLn $ renderSvg (polyline [(1,1), (2,1), (2,2), (3,4)] 0.1 C.red)
--
-- <polyline points="1,1 2,1 2,2 3,4" fill="none" stroke="#ff0000" stroke-width="0.1" />
polyline :: (Show a1, Show a) => [(a1, a)] -> Double -> C.Colour Double -> Svg
polyline lis sw col = S.polyline ! S.points (S.toValue $ unwords $ map showP2 lis) ! S.fill noFill ! S.stroke (colourAttr col )! S.strokeWidth (vd sw) ! S.strokeLinejoin (S.toValue ("round" :: String))

showP2 :: (Show a, Show a1) => (a1, a) -> String
showP2 (x, y) = show x ++ "," ++ show y 


noFill :: S.AttributeValue
noFill = S.toValue ("none" :: String)


  
-- * Helpers


-- | Given a point `x` in a range [x1min, x1max], map it by affine transformation onto the interval [x2min, x2max]
affine :: Fractional t => t -> t -> t -> t -> t -> t
affine x2min x2max x1min x1max x = (x - x1min)*d2/d1 + x2min where
  d1 = x1max - x1min
  d2 = x2max - x2min

  


-- Render a Colour from `colour` into a `blaze` Attribute
colourAttr :: C.Colour Double -> S.AttributeValue
colourAttr = S.toValue . C.sRGB24show 

-- ** Conversion from primitive numerical types to AttributeValue
vi :: Int -> S.AttributeValue
vi = S.toValue


-- | For use e.g. in `viewbox`
vis :: [Int] -> S.AttributeValue
vis = S.toValue . unwords . map show

vd :: Double -> S.AttributeValue
vd = S.toValue

vds :: [Double] -> S.AttributeValue
vds = S.toValue . unwords . map show




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
