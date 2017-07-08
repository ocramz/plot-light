module Data.TimeSeries.Forex where

import Data.Time

data FxRow a  = FxRow {
    rateOpen :: a
  , rateHigh :: a
  , rateLow :: a
  , rateClose :: a
               } deriving (Eq, Show)


-- | An instant, defined by date (Day) and TimeOfDay
data Tick = Tick Day TimeOfDay
  deriving (Eq, Show, Ord)

-- | A point in a time series
data TsPoint a =
  Tsp {
    _tick :: Tick,
    _val :: a
    } deriving (Eq, Show)
