module Graphics.Rendering.Plot.Light.PlotTypes.TimeSeries where

import GHC.Real
import Data.Fixed (Pico)
import Data.Time
import qualified Data.Text as T

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Types
import Data.TimeSeries.Forex


-- | Compute the plotting coordinates of a timeseries point

-- | Preprocess the dataset for plotting 
-- 1. Remap the figure data to fit within the FigData ranges, expressed in pixels
-- 2. Flip the data along the y axis since the origin in SVG is the top-left corner of the screen





-- * Helpers


-- | Create a Tick from valid (year, month, day, hour, minute, second)
mkTick :: Integer -> Int -> Int -> Int -> Int -> Pico -> Maybe Tick
mkTick yy mm dd hr mi se = do
   tim <- makeTimeOfDayValid hr mi se
   let d = fromGregorian yy mm dd
   return $ Tick d tim


-- | Map a Tick onto the rationals
fromTick :: Tick -> Rational
fromTick (Tick d t) = fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction t
    

tspToTuple :: (a -> b) -> TsPoint a -> (Float, b)
tspToTuple f tsp = (tickToFloat tsp, f $ _val tsp)
  
tickToFloat :: TsPoint a -> Float
tickToFloat = fromRational . fromTick . _tick




