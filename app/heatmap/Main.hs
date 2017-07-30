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
import qualified Data.Colour.Palette.Harmony as CH


fname = "data/heatmap-bw"

xPlot = 400
yPlot = 300
fnameOut = "data/heatmap-1.svg"

fdat :: FigureData Rational
fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10

palette0 = palette [C.red, C.white, C.blue] 15


main :: IO ()
main = do
  dat <- T.readFile fname
  let pd = A.parseOnly (gridNum space) dat
  case pd of Left e -> error e
             Right d -> do
                  let pixels = heatmap fdat palette0 d
                      svg_t = svgHeader xPlot yPlot pixels
                  -- putStrLn $ renderSvg svg_t
                  T.writeFile fnameOut $ T.pack $ renderSvg svg_t      


plotFun2ex1 = do
  let 
    p1 = Point (-2) (-2)
    p2 = Point 2 2
    frame = mkFrame p1 p2
    nx = 50 
    ny = 50
    f x y = x'**2 - y'**2 where
      (x', y') = (fromRational x, fromRational y)
    lps = plotFun2 f $ meshGrid frame nx ny
    pixels = heatmap' fdat palette0 frame nx ny lps
    svg_t = svgHeader xPlot yPlot pixels
  T.writeFile "data/heatmap-2.svg" $ T.pack $ renderSvg svg_t  
  

plotFun2ex1_1 = 
  let 
    p1 = Point (-2) (-2)
    p2 = Point 2 2
    nx = 50 
    ny = 50
    f x y = cos ( pi * theta ) * sin r 
      where
      r = x'**2 + y'**2
      theta = atan2 y'  x'
      (x', y') = (fromRational x, fromRational y)
    fname = "data/heatmap-3.svg"
  in
    withMeshGrid p1 p2 nx ny $ \fr gr -> do
      let
        lps = plotFun2 f gr
        pixels = heatmap' fdat palette0 fr nx ny lps
        svg_t = svgHeader xPlot yPlot pixels
      T.writeFile fname $ T.pack $ renderSvg svg_t  
        

withMeshGrid :: (RealFrac a, Enum a) =>
    Point a -> Point a -> a -> a -> (Frame a -> [Point a] -> t) -> t
withMeshGrid p1 p2 nx ny f =
  withFrame p1 p2 $ \fr -> let
  grid = meshGrid fr nx ny in f fr grid 

withFrame p1 p2 f = f frame where
  frame = mkFrame p1 p2




-- parsers

-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
