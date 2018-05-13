{-# language FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Histogram (histogramD, densityD) where

import Graphics.Rendering.Plot.Light.Internal

import Control.Monad (forM_)

import Text.Blaze.Svg
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
-- import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C

import qualified Data.Histogram as H (Histogram(..), Bin(..), bins, asList)
import qualified Data.Histogram.Bin as H (BinD(..), binD, BinI(..), binI, binSize, UniformBin(..))
import qualified Data.Histogram.Fill as H (mkSimple, mkWeighted, fillBuilder, HBuilder(..))

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import qualified Data.Text as T
import qualified Data.Text.IO as T



xPlot = 400
yPlot = 300
fnameOut = "data/histogram-1.svg"

dats = [1,2,1,3,46,30,4,7,73,12,23,90,34,24,5,6,12,3,55,61,70,80,75,90,65,68]

main = do
  let
    kol = shapeColNoBorder C.red 1
    svg_t = svgHeader xPlot yPlot $ histogramD kol 5 dats
  T.writeFile fnameOut $ T.pack $ renderSvg svg_t




histogramD :: Foldable v =>
             ShapeCol Double -- ^ Colour information (fill, stroke, opacity)
          -> Int             -- ^ Number of histogram bins
          -> v Double        -- ^ Data
          -> Svg
histogramD col nBins dats = forM_ pshs $ \(p, h) -> rectCenteredMidpointBase binW (hMult * h) col p where
  his = histo nBins dats
  p1 = Point (head binCenters) 0
  p2 = Point (last binCenters) 0
  ps = pointRange nBins p1 p2
  pshs = zip ps binCounts 
  (binCenters, binCounts) = unzip $ H.asList his
  binW = H.binSize $ H.bins his  -- bin width
  hMult = 10 -- height multiplication coeff. (hack)

-- | Normalized histogram counts (i.e. uniform density approximation) 
densityD :: (Fractional b, VU.Unbox b, Foldable v) =>
            Int
         -> v Double
         -> [(Double, b)]
densityD n = density . histo n

density :: (Fractional b, VU.Unbox b, H.Bin bin) =>
           H.Histogram bin b
        -> [(H.BinValue bin, b)] -- ^ (Bin centers, Normalized bin counts)
density hist = zip binCenters ((/ nelems) `map` binCounts)where
  (binCenters, binCounts) = unzip $ H.asList hist
  nelems = sum binCounts


-- | Uniform, un-weighted bins
histo :: (Foldable v, VU.Unbox a, Num a) =>
         Int     -- ^ Number of bins
      -> v Double -- ^ Data
      -> H.Histogram H.BinD a
histo n v = histo'' bins v where
  mi = minimum v
  ma = maximum v + 1
  bins = H.binD mi n ma


histo'' :: (Foldable f, H.Bin bin, VU.Unbox val, Num val) =>
           bin
        -> f (H.BinValue bin)
        -> H.Histogram bin val
histo'' bins v = H.fillBuilder buildr v
  where
    buildr = H.mkSimple bins
