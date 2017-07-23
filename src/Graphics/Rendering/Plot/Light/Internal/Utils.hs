module Graphics.Rendering.Plot.Light.Internal.Utils where


import Data.Scientific (Scientific, toRealFloat)

-- * Misc helpers




-- ** Numeric formats

toFloat :: Scientific -> Float
toFloat x = toRealFloat x :: Float

-- | Separate whole and decimal part of a fractional number
-- e.g.
--
-- > > wholeDecimal 
wholeDecimal :: (Integral a, RealFrac b) => b -> (a, b)
wholeDecimal x = (w, d) where
  w = floor x
  d = x - fromIntegral w
