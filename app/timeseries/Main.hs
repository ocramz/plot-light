{-# language OverloadedStrings #-}
module Main where

import Control.Arrow ((***), (&&&))

import Graphics.Rendering.Plot.Light.IO.Text
import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Types
import Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries
import Data.TimeSeries.Forex
  
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Scientific (toRealFloat)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Data.Colour.Names as C

both f = f &&& f


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
                  (tmax, tmin) = (maximum &&& minimum) t
                  td = tmax - tmin
                  (xmax, xmin) = (toRealFloat . maximum &&& toRealFloat . minimum) x
                  xd = xmax - xmin
                  fdat = FigData td xd tmin tmax xmin xmax
                -- pure fdat -- (polyline dat 0.1 C.red) 
                  svg_t = renderSvg $ figure fdat
                       (polyline dat 0.1 C.red)
                T.writeFile fnameOut $ T.pack svg_t


-- putStrLn $ renderSvg (polyline [(1,1), (2,1), (2,2), (3,4)] 0.1 C.red)


tspToTuple :: (a -> b) -> TsPoint a -> (Double, b)
tspToTuple f tsp = (tickToDouble tsp, f $ _val tsp) where
  
tickToDouble :: TsPoint a -> Double
tickToDouble = fromRational . fromTick . _tick
