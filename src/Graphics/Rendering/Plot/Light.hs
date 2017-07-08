{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light where

-- import Data.Foldable
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Blaze.Svg
import Text.Blaze.Svg11 ((!), mkPath, rotate, translate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

-- data BoundingBox a = BBox { _x0 :: a, _y0 :: a, _width :: a, _height :: a} deriving (Eq, Show)


data FigureData a =
  FigData {
    _width :: Int
  , _height :: Int
  , _xmin :: Int
  , _xmax :: Int
  , _ymin :: Int
  , _ymax :: Int
  , _figData :: a
      }


figure :: FigureData a -> Svg -> Svg
figure fd =
  S.docTypeSvg
  ! A.version "1.1"
  ! A.width (vi $ _width fd)
  ! A.height (vi $ _height fd)
  ! A.viewbox (vis [_xmin fd, _ymin fd, _xmax fd, _ymax fd])


-- a filled rectangle
rect :: Int -> Int -> Int -> Int -> C.Colour Double -> Svg
rect x0 y0 wid hei col = S.g ! A.transform (translate x0 y0) $ 
  S.rect ! A.width (vi wid) ! A.height (vi hei) ! A.fill (colAttr col)

-- Render a Colour from `colour` into a `blaze` Attribute
colAttr :: C.Colour Double -> S.AttributeValue
colAttr = S.toValue . C.sRGB24show 

vi :: Int -> S.AttributeValue
vi = S.toValue

vis :: [Int] -> S.AttributeValue
vis = S.toValue . unwords . map show







main :: IO ()
main = do
  let a = renderSvg svgDoc
  putStrLn a

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
    S.g ! A.transform makeTransform $ do
      -- S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
      -- S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
      -- S.path ! A.d makePath

makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5

makeTransform :: S.AttributeValue
makeTransform = translate 1 1 -- rotate 50
