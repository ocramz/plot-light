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

tsAxis fval wfig hfig sw col1 col2 rot ps = do                            
  axis oSvg X (right - left) sw col1 0.01 Continuous 10 rot TAEnd (T.pack . fst) (V2 (-10) 0) dat'
  axis oSvg Y (top - bot) sw col1 0.01 Continuous 10 0 TAEnd (T.pack . snd) (V2 (-10) 0) dat'
  polyline (_lp <$> dat') sw Continuous Round col2
  where
    dat = tspToLP fval (\(Tick d t) v -> (show (d, t), show (fval v))) <$> ps
    dat' = toSvgFrameLP from to False <$> dat  
    (left, right) = (0.1 * wfig, 0.9 * wfig)
    (top, bot) = (0.1 * hfig, 0.9 * hfig)
    oTo = Point left top
    p2To = Point right bot
    from = frameFromPoints $ _lp <$> dat
    to = mkFrame oTo p2To
    oSvg = Point left bot
    
    



tsAxis1 fd sw colAxis colData rot plabx plaby ps =
  toPlot fd T.pack T.pack rot 0 sw colAxis plabx plaby fplot ps where
    fplot lps = polyline (_lp <$> lps) sw Continuous Round colData
  

svg1 = renderSvg $ tsAxis1 fdat 2 C.black C.red (-45) (Just ptx) (Just pty) dat1

svg2 = renderSvg $ tsAxis1 fdat 2 C.black C.red (-45) Nothing (Just pty) dat1


fdat = FigureData 400 300 0.1 0.9 0.1 0.85 10

dat1 :: [ LabeledPoint String Double ]
dat1 = [LabeledPoint (Point 0 0) "blah",
        LabeledPoint (Point 0 1) "asdf",
        LabeledPoint (Point 1 1) "yo",
        LabeledPoint (Point 1 2) "blap",
        LabeledPoint (Point 2 2) "chow"]

ptx = labelPoint (show . _px) <$> pointRange 2 (Point 0 0) (Point 2 0)
pty = labelPoint (show . _py) <$> pointRange 2 (Point 0 0) (Point 0 2)

to, from :: Frame Double
from = frameFromPoints $ _lp <$> dat1
to = mkFrameOrigin 400 300





  

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
    

-- fromTickI (Tick d t) = toModifiedJulianDay d + timeOfDayToDayFraction t






-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim










-- nth xss n = reverse $ go xss n [] where
--   go (_:xs) i l | i>0  = go xs (i-1) l
--   go (x:xs) i l | i==0 = go xs n (x : l)
--   go [] _ l = l

-- tsp1 :: Maybe (TsPoint (FxRow Double))
-- tsp1 = Tsp <$> mkTick 2017 16 3 20 30 01 <*> Just (FxRow pi 20 10 5.4)
