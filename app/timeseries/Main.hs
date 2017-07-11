{-# language OverloadedStrings #-}
module Main where

-- import Control.Arrow ((***), (&&&))

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.IO.Text
-- import Graphics.Rendering.Plot.Light.Internal
-- -- import Graphics.Rendering.Plot.Light.Internal.Types
import Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries
import Data.TimeSeries
import Data.TimeSeries.Forex
  
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Scientific (Scientific, toRealFloat)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Data.Colour.Names as C



fname = "data/forex"

xPlot = 400
yPlot = 300
fnameOut = "data/forex_plot.svg"

-- figData = mkFigureData 10 10 xPlot yPlot

-- main = print "hello!"

main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right datarows -> putStrLn $ renderSvg (tsAxis rateOpen xPlot yPlot 3 C.red (-45) datarows)
--                   dat = tspToTuple rateHigh <$> reverse datarows
--                   (dat', fdat) = mkFigureData xPlot yPlot toFloat dat
--                   svg_t = renderSvg $ figure fdat
--                        (polyline dat' 0.5 C.red)
--                 T.writeFile fnameOut $ T.pack svg_t

-- toFloat :: Scientific -> Float
-- toFloat x = toRealFloat x :: Float


