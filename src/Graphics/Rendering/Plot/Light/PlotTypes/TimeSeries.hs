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
import Data.TimeSeries.Forex

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
tsAxis fval wFig hFig sw col rot ps = axis (Point 0 200) X wFig sw col 0.01 Continuous 15 rot TAEnd T.pack (V2 (-10) 0) dat'
  where
    lpFun = tspToLP fval (\(Tick d t) _ -> show (d, t))
    dat = lpFun <$> ps
    from = frameFromPoints $ _lp <$>  dat
    to = mkFrameOrigin wFig hFig
    dat' = moveLabeledPointV2Frames from to False True <$> dat

-- tsAxis fval wFig hFig ps = dat' 
--   where
--     lpFun = tspToLP fval (\t _ -> show t)
--     dat = lpFun <$> ps
--     from = frameFromPoints $ _lp <$> dat
--     to = mkFrameOrigin wFig hFig
--     dat' = moveLabeledPointV2Frames from to True True <$> dat



-- nth xss n = reverse $ go xss n [] where
--   go (_:xs) i l | i>0  = go xs (i-1) l
--   go (x:xs) i l | i==0 = go xs n (x : l)
--   go [] _ l = l

-- tsp1 :: Maybe (TsPoint (FxRow Double))
-- tsp1 = Tsp <$> mkTick 2017 16 3 20 30 01 <*> Just (FxRow pi 20 10 5.4)

-- dat1 :: [ LabeledPoint String Double ]
-- dat1 = [LabeledPoint (Point 58000 pi) "blah",
--         LabeledPoint (Point 59000 2.3) "asdf",
--         LabeledPoint (Point 58500 3.42) "yo"]

-- to, from :: Frame Double
-- from = frameFromPoints $ _lp <$> dat1
-- to = mkFrameOrigin 400 300



-- dat1' = moveLabeledPointV2Frames from to False False <$> dat1



  

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


