module Data.TimeSeries.Forex where



data FxRow a  = FxRow {
    rateOpen :: a
  , rateHigh :: a
  , rateLow :: a
  , rateClose :: a
               } deriving (Eq, Show)



