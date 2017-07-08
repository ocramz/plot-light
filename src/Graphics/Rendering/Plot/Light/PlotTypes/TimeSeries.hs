module Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries where

import Data.Time
import qualified Data.Text as T



-- | A point in a time series

data Tick =
    D Day
  | T TimeOfDay
  | DT Day TimeOfDay
  deriving (Eq, Show)

data TsPoint a =
  Tsp {
    _tick :: Tick,
    _value :: a
    } deriving (Eq, Show)

showTick :: Tick -> T.Text
showTick (D d) = T.pack $ show d
showTick (T t) = T.pack $ show t
showTick (DT d t) = T.pack $ unwords [show d, show t]


