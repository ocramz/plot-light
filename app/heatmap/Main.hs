module Main where

-- import Control.Monad (forM_)
-- import Data.Ratio

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.PlotTypes

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T 
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Scientific (Scientific, toRealFloat)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg

import qualified Data.Colour.Names as C
import qualified Data.Colour as C
import qualified Data.Colour.Palette.BrewerSet as CP



fname = "data/heatmap-bw"

xPlot = 300
yPlot = 300
fnameOut = "data/heatmap-1.svg"

fdat = FigureData xPlot yPlot 0.1 0.9 0.1 0.85 10


nColors = 9  -- 3 - 9
palette :: [C.Colour Double]
palette = CP.brewerSet CP.GnBu nColors

main :: IO ()
main = do
  dat <- T.readFile fname
  let pd = A.parseOnly (gridNum space) dat
  case pd of Left e -> error e
             Right d -> do
                  let pixels = heatmap fdat palette d
                      svg_t = svgHeader (mkFrameOrigin xPlot yPlot) pixels
                  -- putStrLn $ renderSvg svg_t
                  T.writeFile fnameOut $ T.pack $ renderSvg svg_t      



-- parsers

-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
