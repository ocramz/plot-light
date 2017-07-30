module Graphics.Rendering.Plot.Light.PlotTypes.Scatter where

import Graphics.Rendering.Plot.Light.Internal

import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Text.Blaze.Svg
import qualified Data.Colour as C
import qualified Data.Colour.Names as C



data Glyph = Square | Circle | Cross | Plus deriving (Eq, Show)

glyph
  :: (Show a, RealFrac a) =>
     a
     -> a
     -> Glyph
     -> Maybe (C.Colour Double)
     -> Maybe (C.Colour Double)
     -> Point a
     -> Svg
glyph w sw Square scol fcol p = squareCentered w sw scol fcol p
glyph w sw Circle scol fcol p = circle w sw scol fcol p
glyph w sw Cross _ fcol p = crossGlyph w sw (fromMaybe C.black fcol) p
glyph w sw Plus _ fcol p = plusGlyph w sw (fromMaybe C.black fcol) p


scatter figdata = forM_



