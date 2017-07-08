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
