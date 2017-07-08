{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light.Internal where


import Control.Arrow ((&&&), (***))

-- import Data.Foldable
import qualified Data.Text as T
-- import qualified Data.Vector as V

import Text.Blaze.Svg
import Text.Blaze.Svg11 -- ((!), mkPath, rotate, translate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C



data FigureData a d =
  FigData {
    _width :: a
  , _height :: a
  , _xmin :: a
  , _xmax :: a
  , _ymin :: a
  , _ymax :: a
  , _figData :: d
      }


-- | A LabeledPoint carries the information of where a point should be plotted, what label should it carry (e.g. for labelling the axes) and its function value 
data LabeledPoint c l a =
  LabeledPoint { _coord :: c, _label :: l, _value :: a }

data P1 a = P1 a
data P2 a = P2 a a



-- axis f1 figd = undefined
--   where
--     d = f1 <$> _figData figd -- pick out data
--     numd = length d
--     mm = maximum d
--     m = minimum d


-- | Given a point `x` in a range [x1min, x1max], map it by affine transformation onto the interval [x2min, x2max]
affine :: Fractional t => t -> t -> t -> t -> t -> t
affine x2min x2max x1min x1max x = (x - x1min)*d2/d1 + x2min where
  d1 = x1max - x1min
  d2 = x2max - x2min



-- | Header for a Figure
figure :: FigureData Int d -> Svg -> Svg
figure fd =
  S.docTypeSvg
  ! A.version "1.1"
  ! A.width (vi $ _width fd)
  ! A.height (vi $ _height fd)
  ! A.viewbox (vis [_xmin fd, _ymin fd, _xmax fd, _ymax fd])


-- | A filled rectangle, centered at (x0, y0)
rectCentered
  :: Double -> Double -> Double -> Double -> C.Colour Double -> Svg
rectCentered x0 y0 wid hei col = S.g ! A.transform (translate x0c y0c) $ 
  S.rect ! A.width (vd wid) ! A.height (vd hei) ! A.fill (colourAttr col) where
   x0c = x0 - (wid / 2)
   y0c = y0 - (hei / 2)   

-- centeredAt x0 y0 = S.g ! A.transform (translate x0 y0) 



-- * Helpers 

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
