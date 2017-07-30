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

xPlot = 400
yPlot = 300
fnameOut = "data/heatmap-1.svg"

-- fdat :: Num a => FigureData a
fdat :: FigureData Rational
fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10


nColors = 11  -- 3 - 9
palette :: [C.Colour Double]
palette = CP.brewerSet CP.Spectral nColors

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


main' = do
  let 
    p1 = Point (-2) (-1.5)
    p2 = Point 2 1.5
    frame = mkFrame p1 p2
    nx = 100 
    ny = 75
    f x y = x'**2 - sin y' where
      (x', y') = (fromRational x, fromRational y)
    lps = plotFun2 f (meshGrid p1 p2 nx ny)
    pixels = heatmap' fdat palette frame nx ny lps
    svg_t = svgHeader (mkFrameOrigin xPlot yPlot) pixels
  T.writeFile "data/heatmap-2.svg" $ T.pack $ renderSvg svg_t  
  


-- parsers

-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
