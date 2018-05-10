{-# language FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Histogram where

import Graphics.Rendering.Plot.Light.Internal

import Control.Monad (forM_)
-- import Text.Blaze.Svg
import qualified Data.Colour as C
-- import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C

import qualified Data.Histogram.Generic as H (histogram, Histogram(..), Bin(..))
import qualified Data.Histogram.Bin as H (BinD(..), binD, BinI(..), binI)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

histogram figdata = undefined 


-- | Uses the data range as binning range
binDfromVG :: Foldable v => Int -> v Double -> H.BinD
binDfromVG n v = H.binD mi n ma where
  mi = minimum v
  ma = maximum v
  
