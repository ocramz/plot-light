module Main where

import Control.Monad (forM_)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T 
import qualified Data.Text.IO as T (readFile, writeFile)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.PlotTypes

xPlot = 400
yPlot = 300
fnameOut = "data/scatter-1.svg"

fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10

dats = zipWith LabeledPoint p_ l_ where
  l_ = [-5, -4 .. ]
  p_ = zipWith Point [4,7,12,23,90,34,24,5,6,12,3] [43,23,1,23,8,11,17,25,4,5]

spdata = ScatterPointData Circle 3 3 C.red


main = do
  let
    frameTo = frameFromFigData fdat
    frameFrom = frameFromPoints $ _lp <$> dats
    vmin = minimum $ _lplabel <$> dats
    vmax = maximum $ _lplabel <$> dats     
    f l sz = sz + l
    g _ w = tanh w
    h l col = C.blend l' C.blue col
      where
        l' = (l - vmin)/(vmax - vmin)
    dats' = moveLabeledPointBwFrames frameFrom frameTo False False <$> dats
    svg_t = svgHeader xPlot yPlot $ do
      -- axes fdat frameFrom 2 C.black 10 10
      -- scatterLP f g h spdata dats'
      scatterLPBar fdat 50 vmin vmax 3 TopRight 100 f g h spdata
  putStrLn $ renderSvg svg_t
  -- T.writeFile fnameOut $ T.pack $ renderSvg svg_t
    
