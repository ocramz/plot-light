module Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries where

import Data.Fixed (Pico)
import Data.Time
import qualified Data.Text as T

import Graphics.Rendering.Plot.Light.Internal

-- | An instant, defined by date (Day) and TimeOfDay
data Tick = Tick Day TimeOfDay
  deriving (Eq, Show, Ord)

-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim


-- | Map a Tick onto the rationals
fromTick :: Tick -> Rational
fromTick (Tick d t) = fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction t

-- | A point in a time series
data TsPoint a =
  Tsp {
    _tick :: Tick,
    _value :: a
    } deriving (Eq, Show)






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
