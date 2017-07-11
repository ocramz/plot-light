{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries where

import GHC.Real
import Data.Fixed (Pico)
import Data.Time
import Data.Scientific

import qualified Data.Text as T

import Graphics.Rendering.Plot.Light.Internal
import Data.TimeSeries

-- For debugging
import Text.Blaze.Svg
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Text.Blaze.Svg.Renderer.String (renderSvg)


-- | Compute the plotting coordinates of a timeseries point

-- | Preprocess the dataset for plotting 
-- 1. Remap the figure data to fit within the FigData ranges, expressed in pixels
-- 2. Flip the data along the y axis since the origin in SVG is the top-left corner of the screen

-- tsAxis :: (Num a, LinearMap (DiagMat2 a) (V2 a), RealFrac a, Show a,
--                    Foldable t, Functor t) =>
--                   (a -> a)
--                   -> a
--                   -> a
--                   -> a
--                   -> C.Colour Double
--                   -> a
--                   -> t (TsPoint a)
--                   -> Svg
tsAxis fval wFig hFig sw col rot ps = axis origin X wFig sw col 0.01 Continuous rot TAEnd T.pack (V2 (-10) 0) dat
  where
    lpFun = tspToLP fval (\t _ -> show t)
    dat = lpFun <$> ps
    frameFrom = frameFromDataset dat
    frameTo = mkFrameOrigin wFig hFig
    dat' = moveLabeledPointV2Frames frameFrom frameTo (mempty :: DiagMat2 Scientific) (V2 0 0) <$> dat


nth_ (x:_) 0        = x
nth_ (_:xs) n | n>0 = nth_ xs (n-1)

nth xss n = reverse $ go xss n [] where
  go (_:xs) i l | i>0  = go xs (i-1) l
  go (x:xs) i l | i==0 = go xs n (x : l)
  go [] _ l = l

-- * Helpers

-- | Create a `LabeledPoint` from a time series point (`TsPoint`). The `_tick` (time axis) field will be used for the x coordinate, whereas both fields of TsPoint may be used to create the label field.
--
-- NB : The coordinates of the resulting LabelPoint still live in the original data space; they must be rescaled to fit in the figure viewport
tspToLP :: Fractional a => 
     (t -> a)
  -> (Tick -> t -> l)
  -> TsPoint t
  -> LabeledPoint l a
tspToLP fy g = LabeledPoint <$> pf <*> lf where
  pf = Point <$> tickToFractional <*> fy . _val
  lf = g <$> _tick <*> _val
  
  
tickToFractional :: Fractional b => TsPoint a -> b
tickToFractional = fromRational . fromTick . _tick

-- | Map a Tick onto the rationals
fromTick :: Tick -> Rational
fromTick (Tick d t) = fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction t
    




-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim


