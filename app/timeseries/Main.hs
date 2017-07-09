{-# language OverloadedStrings #-}
module Main where

import Graphics.Rendering.Plot.Light.IO.Text
import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Types
import Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries
import Data.TimeSeries.Forex
  
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Data.Colour.Names as C


fname = "data/forex"

fnameOut = "data/forex_plot.svg"

main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right datarows -> do
                let
                  dat = tspToTuple rateHigh <$> reverse datarows
                  (t, x) = unzip dat
                  tmin = minimum t
                  tmax = maximum t
                  td = tmax - tmin
                  xmin = minimum x
                  xmax = maximum x
                  xd = xmax - xmin
                  fdat = FigData td xd tmin tmax xmin xmax
                  svg_t = renderSvg $ figure fdat (polyline dat 0.1 C.red)
                T.writeFile fnameOut $ T.pack svg_t


-- putStrLn $ renderSvg (polyline [(1,1), (2,1), (2,2), (3,4)] 0.1 C.red)


tspToTuple :: (a -> b) -> TsPoint a -> (Double, b)
tspToTuple f tsp = (tickToDouble tsp, f $ _val tsp) where
  
tickToDouble :: TsPoint a -> Double
tickToDouble = fromRational . fromTick . _tick
