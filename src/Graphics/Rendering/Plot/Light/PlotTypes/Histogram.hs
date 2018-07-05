{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Histogram where

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
fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10
frameTo = frameFromFigData fdat

dats = [1,2,46,30,4,7,73,12,23,90,34,24,5,6,12,3,55,61,70,80,75,90,65,68,34,23,32,31,15,16,17,90,91,98,97,98,6,30,4,7,73,12,23,90,34,24,5,6,12,3,55,61,7]

-- dats = [1,1,1,2,2,3,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6]


main = error "undefined"

-- main = do
--   let
--     kol = shapeColNoBorder C.red 1
--     -- svg_t = svgHeader xPlot yPlot $ histogramD' frameTo kol 10 dats
--   T.writeFile fnameOut $ T.pack $ renderSvg svg_t


-- | Returns the 'Frame' associated with a 'Histogram' along with the bins as 'LabeledPoint's (where the point coordinates lie on the X axis and the label contains the histogram count)
histGeometry :: (bv ~ H.BinValue bin, VU.Unbox bv, H.Bin bin, Ord bv, Num bv) =>
                H.Histogram bin bv
             -> (Frame bv, [LabeledPoint bv bv])
histGeometry hist = (frm, hlps) where
  hl = H.asList hist
  hlps = map (\(x, bc) -> let p = Point x 0 in mkLabeledPoint p bc) hl
  (binCenters, binCounts) = unzip hl
  maxCount = maximum binCounts
  x1 = head binCenters
  x2 = last binCenters
  p1 = Point x1 0
  p2 = Point x2 maxCount
  frm = mkFrame p1 p2


-- histogramD' :: Foldable v =>
--                Frame Double
--             -> ShapeCol Double
--             -> Int
--             -> v Double
--             -> Svg
-- histogramD' frameTo col n dats = do
--   axes fdat (frameFromFigData fdat) 1 C.black 10 10 
--   toBottomLeftSvgOrigin fdat $ 
--     forM_ lps' $ \(LabeledPoint p l) -> rectCenteredMidpointBase binw' (hMult * l) col p
--   where
--     hist = histo n dats
--     (frameFrom, lps) = histGeometry hist
--     lps' = moveLabeledPointBwFrames frameFrom frameTo False False `map` lps
--     binw = H.binSize $ H.bins hist  -- bin width
--     hMult = 0.9 * sy -- height multiplication coeff. (hack)
--     (sx, sy) = fromToStretchRatios frameFrom frameTo
--     binw' = sx * binw
    






-- histogramD :: Foldable v =>
--              ShapeCol Double -- ^ Colour information (fill, stroke, opacity)
--           -> Int             -- ^ Number of histogram bins
--           -> v Double        -- ^ Data
--           -> Svg
-- histogramD col nBins dats = forM_ pshs $ \(p, h) -> rectCenteredMidpointBase binW (hMult * h) col p where
--   his = histo nBins dats
--   p1 = Point (head binCenters) 0
--   p2 = Point (last binCenters) 0
--   ps = pointRange nBins p1 p2
--   pshs = zip ps binCounts 
--   (binCenters, binCounts) = unzip $ H.asList his
--   binW = H.binSize $ H.bins his  -- bin width
--   hMult = 10 -- height multiplication coeff. (hack)



