module Graphics.Rendering.Plot.Light.Internal.Utils where

import qualified Data.Colour as C
import Data.Scientific (Scientific, toRealFloat)

-- * Misc helpers


-- | `blendTwo c1 c2 n` creates a palette of `n` intermediate colours, interpolated linearly between `c1` and `c2`.
blendTwo :: C.Colour Double -> C.Colour Double -> Int -> [C.Colour Double]
blendTwo c1 c2 n = blf c1 c2 <$> [0, dh .. 1] where
  dh = 1/fromIntegral n
  blf cola colb x = C.blend x cola colb

-- | `palette cs n` blends linearly a list of colours `cs`, by generating `n` intermediate colours between each consecutive pair.
palette :: [C.Colour Double] -> Int -> [C.Colour Double]
palette ll n = concatMap f l1 where
  l1 = zip ll (tail ll)
  f (c1, c2) = blendTwo c2 c1 n



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
