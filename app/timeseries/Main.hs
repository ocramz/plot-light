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



fname = "data/forex_small"

xPlot = 800
yPlot = 600
fnameOut = "data/forex_plot_3.svg"


main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right d -> -- print $ tsAxis (toFloat . rateOpen) xPlot yPlot 3 C.blue (-45) d
               do
               let svg_t = svgHeader (mkFrameOrigin xPlot yPlot) $ tsAxis avgTs xPlot yPlot 3 C.black C.red (-45) d
               -- putStrLn $ renderSvg svg_t
               T.writeFile fnameOut $ T.pack $ renderSvg svg_t

toFloat :: Scientific -> Float
toFloat x = toRealFloat x :: Float


avgTs :: FxRow Scientific -> Float
avgTs x = 0.5 * (h + l) where
  h = toFloat (rateHigh x)
  l = toFloat (rateLow x)
