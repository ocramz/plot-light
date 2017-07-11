module Data.TimeSeries where

import Data.Time

-- | An instant, defined by date (Day) and TimeOfDay
data Tick = Tick Day TimeOfDay
  deriving (Eq, Show, Ord)

-- | A point in a time series
data TsPoint a =
  Tsp {
    _tick :: Tick,
    _val :: a
    } deriving (Eq, Show)
