module Graphics.Rendering.Plot.Light.Internal.Utils where




-- * Misc helpers

-- | Separate whole and decimal part of a fractional number
-- e.g.
--
-- > > wholeDecimal 
wholeDecimal :: (Integral a, RealFrac b) => b -> (a, b)
wholeDecimal x = (w, d) where
  w = floor x
  d = x - fromIntegral w
