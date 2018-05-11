{-# language FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Histogram where

import Graphics.Rendering.Plot.Light.Internal

import Control.Monad (forM_)
-- import Text.Blaze.Svg
import qualified Data.Colour as C
-- import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C

import qualified Data.Histogram as H (Histogram(..), Bin(..), asList)
import qualified Data.Histogram.Bin as H (BinD(..), binD, BinI(..), binI)
import qualified Data.Histogram.Fill as H (mkSimple, mkWeighted, fillBuilder, HBuilder(..))

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

histogram figdata = undefined 




-- | Uses data range as binning range
histo :: (Foldable v, VU.Unbox a, Num a) =>
         Int
      -> v Double
      -> H.Histogram H.BinD a
histo n v = H.fillBuilder buildr v
  where
    mi = minimum v
    ma = maximum v
    bins = H.binD mi n ma
    buildr = H.mkSimple bins
  
