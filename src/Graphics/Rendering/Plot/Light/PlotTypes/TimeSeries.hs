module Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries where

import GHC.Real
import Data.Fixed (Pico)
import Data.Time
import qualified Data.Text as T

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Types
import Data.TimeSeries.Forex



-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim


-- | Map a Tick onto the rationals
fromTick :: Tick -> Rational
fromTick (Tick d t) = fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction t


-- | Transform the time coordinate of a timeseries point 
mapPointToViewbox :: (Fractional t, Fractional a) =>
                           (px -> Rational)    -- | From the FigureData type
                           -> (Rational -> t)  -- | To the time axis type
                           -> (Rational -> a)  -- | To the value type
                           -> FigureData px   
                           -> Tick             -- | Min. time
                           -> Tick             -- | Max. time
                           -> a                -- | Min. value
                           -> a                -- | Max. value
                           -> TsPoint Rational
                           -> LabeledPoint t Tick a
mapPointToViewbox ff fx fy figdat xmin xmax ymin ymax p = LabeledPoint t' (_tick p) p' 
  where
    t' = affine (fx . ff $ _xmin figdat) (fx . ff $ _xmax figdat) (fx $ fromTick xmin) (fx $ fromTick xmax) (fx . fromTick $ _tick p)
    p' = affine (fy . ff $ _ymin figdat) (fy . ff $ _ymax figdat) ymin ymax (fy $ _val p)




-- SVG with text

-- <?xml version="1.0" standalone="no"?>
-- <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
--   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
-- <svg width="10cm" height="3cm" viewBox="0 0 1000 300"
--      xmlns="http://www.w3.org/2000/svg" version="1.1">
--   <desc>Example text01 - 'Hello, out there' in blue</desc>

--   <text x="250" y="150" 
--         font-family="Verdana" font-size="55" fill="blue" >
--     Hello, out there
--   </text>

--   <!-- Show outline of canvas using 'rect' element -->
--   <rect x="1" y="1" width="998" height="298"
--         fill="none" stroke="blue" stroke-width="2" />
-- </svg>
