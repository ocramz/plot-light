{-# language FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Histogram where

import Graphics.Rendering.Plot.Light.Internal

import Control.Monad (forM_)
-- import Text.Blaze.Svg
import qualified Data.Colour as C
-- import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C

import qualified Data.Histogram as H (Histogram(..), Bin(..), bins, asList)
import qualified Data.Histogram.Bin as H (BinD(..), binD, BinI(..), binI, binSize, UniformBin(..))
import qualified Data.Histogram.Fill as H (mkSimple, mkWeighted, fillBuilder, HBuilder(..))

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

-- histogram figdata =
  
 



histogram col h = undefined where
  (binCenters, binCounts) = unzip $ H.asList h
  binXOffset = - head binCenters + 0.5 * binSize
  binSize = H.binSize $ H.bins h
  -- hcol = case col of
  --   NoBorderCol c a 



-- | Uniform, un-weighted bins
histo :: (Foldable v, VU.Unbox a, Num a) =>
         Int
      -> v Double
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
