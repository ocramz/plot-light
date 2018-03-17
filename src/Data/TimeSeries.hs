module Data.TimeSeries where

import Data.Time
import Data.Fixed (Pico)
import Graphics.Rendering.Plot.Light.Internal.Utils

-- | An instant, defined by date (Day) and TimeOfDay
data Tick = Tick Day TimeOfDay
  deriving (Eq, Show, Ord)

-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim

-- | A point in a time series
data TsPoint a =
  Tsp {
    _tick :: Tick,
    _val :: a
    } deriving (Eq, Show)


tickToFractional :: Fractional b => TsPoint a -> b
tickToFractional = fromRational . fromTick . _tick

-- | Map a Tick onto the rationals
fromTick :: Tick -> Rational
fromTick (Tick d t) = fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction t
    
-- | Map a rational onto a Tick
toTick :: Rational -> Tick
toTick n = Tick d t
  where
    t = dayFractionToTimeOfDay dec
    d = ModifiedJulianDay wh
    (wh, dec) = wholeDecimal n


hourTick, halfHourTick, quarterHourTick :: Rational
hourTick = 1/24
halfHourTick = 1/2 * hourTick
quarterHourTick = 1/4 * hourTick


locTime :: IO LocalTime
locTime = do
  tz <- getCurrentTimeZone
  ct <- getCurrentTime
  return $ utcToLocalTime tz ct




 

-- tickRange :: Tick -> Tick -> Rational -> [Tick]
-- tickRange t1 t2 dt = toTick <$> [td1, td1 + dt .. td2] where
--   td1 = fromTick t1
--   td2 = fromTick t2
