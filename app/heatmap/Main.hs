module Main where

import Control.Monad (forM_)
import Data.Ratio

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

-- main = print "hello"


fname = "data/heatmap-bw"

xPlot = 800
yPlot = 600
fnameOut = "data/heatmap-1.svg"

fdat = FigureData xPlot yPlot 0.1 0.9 0.1 0.85 10


nColors = 9  -- 3 - 9
palette :: [C.Colour Double]
palette = CP.brewerSet CP.GnBu nColors

main = do
  dat <- T.readFile fname
  let pd = A.parseOnly (gridNum space) dat
  case pd of Left e -> error e
             Right d -> do
               let (nh, nw, vmin, vmax, d') = prepData d
                   w = xPlot / nw
                   h = yPlot / nh
                   from = Frame (Point 0 0) (Point 1 1) :: Frame (Ratio Integer)
                   to = frameFromFigData fdat :: Frame (Ratio Integer)
               -- return $ moveLabeledPointBwFrames from to False False . mapLabel toFloat . fromRationalLP <$> d'
--                    
                   pixels = forM_ d' (mkPixel w h vmin vmax . remap from to)
               -- return pixels
                   svg_t = svgHeader (mkFrameOrigin xPlot yPlot) pixels
--                -- putStrLn $ renderSvg svg_t
               T.writeFile fnameOut $ T.pack $ renderSvg svg_t   

-- remap :: Fractional a =>
--     Frame a -> Frame a -> LabeledPoint Scientific Rational -> LabeledPoint Float a
remap from to = moveLabeledPointBwFrames from to False False . fromRationalLP

fromRationalLP :: Fractional a => LabeledPoint l Rational -> LabeledPoint l a
fromRationalLP (LabeledPoint (Point x y) l) = LabeledPoint (Point (fromRational x) (fromRational y)) l

-- mkPixel :: (RealFrac t, RealFrac a, Show a) =>
--                  a -> a -> t -> t -> LabeledPoint t a -> Svg
mkPixel w h vmin vmax (LabeledPoint p l) = rect w h 0 Nothing (Just col) p where
  col = pickColor (toFloat vmin) (toFloat vmax) (toFloat l)
  

pickColor :: RealFrac t => t -> t -> t -> C.Colour Double
pickColor xmin xmax x = palette !! i
  where
    i = floor (x01 * fromIntegral (nColors - 1))
    x01 = (x-xmin)/(xmax - xmin)



  
  


-- prepData :: [[l]] -> (Rational, Rational, [LabeledPoint l Rational])
prepData ll = (nh, nw, valMin, valMax, d')
  where
    nh = toRational $ length ll
    nw = toRational $ length (head ll)
    d' = toUnitFramedLP nw nh <$> toCoord ll
    valMin = minimum $ _lplabel <$> d'
    valMax = maximum $ _lplabel <$> d'
    
   

toCoord :: (Num i, Enum i) => [[c]] -> [(i, i, c)]
toCoord ll = concat $ reverse $ go 0 ll [] where
  go i (x:xs) acc = go (i + 1) xs $ zip3 (repeat i) [0 ..] x : acc
  go _ [] acc = acc

toUnitFramedLP :: (Fractional t) =>
      t -> t -> (t, t, l) -> LabeledPoint l t
toUnitFramedLP w h (i, j, x) = LabeledPoint p x
  where p = Point (i/h) (j/w)



-- parsers

-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
