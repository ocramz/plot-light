module Main where

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.PlotTypes

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T 
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Scientific (Scientific, toRealFloat)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Data.Colour.Names as C

main = putStrLn "hello!"



fname = "data/heatmap-bw"

xPlot = 800
yPlot = 600
fnameOut = "data/heatmap-1.svg"

fdat = FigureData xPlot yPlot 0.1 0.9 0.1 0.85 10

-- main = do
--   d <- T.readFile fname






-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
