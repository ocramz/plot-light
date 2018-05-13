module Main where

import Control.Monad (forM_)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T 
import qualified Data.Text.IO as T (readFile, writeFile)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.Internal.Geometry

import Data.Parsers



xPlot = 400
yPlot = 300
fnameOut = "data/scatter-2.svg"

fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10

dats = zipWith LabeledPoint p_ l_ where
  l_ = [-5, -4 .. ]
  p_ = zipWith Point [46,30,4,7,73,12,23,90,34,24,5,6,12,3,55,61] [20,35,43,23,20,1,23,8,11,17,25,4,5,26, 30]

spdata = ScatterPointData Plus 3 3 C.black 0.8


main :: IO ()
main = do
  let
    frameTo = frameFromFigData fdat
    frameFrom = frameFromPoints $ _lp <$> dats
    vmin = minimum $ _lplabel <$> dats
    vmax = maximum $ _lplabel <$> dats     
    f l sz = 15 / (1 + exp(- (0.3 * x)) )
      where x = l + sz
    g l w = w * (1 + l / (1 + abs l))
    h l col = C.blend l' C.red col
      where
        l' = (l - vmin)/(vmax - vmin)
    i l alp = alp * ( 1 + l / (1 + abs l))
    dats' = moveLabeledPointBwFrames frameFrom frameTo False True <$> dats
    svg_t = svgHeader xPlot yPlot $ do
      axes fdat frameFrom 2 C.black 10 10
      scatterLP f g h i spdata dats'
      scatterLPBar fdat 50 vmin vmax 3 TopRight 100 f g h i spdata
  -- putStrLn $ renderSvg svg_t
  T.writeFile fnameOut $ T.pack $ renderSvg svg_t
    
