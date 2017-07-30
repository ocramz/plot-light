module Graphics.Rendering.Plot.Light.PlotTypes.Scatter where

import Graphics.Rendering.Plot.Light.Internal

import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Text.Blaze.Svg
import qualified Data.Colour as C
import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C



data GlyphShape = Square | Circle | Cross | Plus deriving (Eq, Show, Enum)

-- | Scatterplot glyph shapes
glyph
  :: (Show a, RealFrac a) =>
     a
     -> a
     -> GlyphShape
     -> Maybe (C.Colour Double)
     -> Maybe (C.Colour Double)
     -> Point a
     -> Svg
glyph w sw Square scol fcol p = squareCentered w sw scol fcol p
glyph w sw Circle scol fcol p = circle w sw scol fcol p
glyph w sw Cross _ fcol p = crossGlyph w sw (fromMaybe C.black fcol) p
glyph w sw Plus _ fcol p = plusGlyph w sw (fromMaybe C.black fcol) p


-- | Utility function for cycling glyph colours and shapes (i.e. unique combinations of these make it easy to tell different datasets apart)
cycleGlyphCols :: (CP.ColorCat, Int) -> Int -> [(CP.Kolor, GlyphShape)]
cycleGlyphCols (pal, n) nsets = take nsets $ zip (cycle $ CP.brewerSet pal n ) (cycle [Square, Circle ..])


scatter figdata = forM_



