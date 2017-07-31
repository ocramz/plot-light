module Graphics.Rendering.Plot.Light.PlotTypes.Scatter where

import Graphics.Rendering.Plot.Light.Internal

import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Text.Blaze.Svg
import qualified Data.Colour as C
import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C


scatter
  :: (Foldable t, Show a, RealFrac a) =>
     ScatterPointData a
     -> t (Point a)
     -> Svg
scatter (ScatterPointData glshape w sw fcol) ps = 
  forM_ ps $ glyph w sw glshape Nothing (Just fcol)

scatterLP
  :: (Foldable t, RealFrac a, Show a) =>
     (l -> b -> a)
     -> (l -> b -> a)
     -> (l -> C.Colour Double -> C.Colour Double)
     -> ScatterPointData b
     -> t (LabeledPoint l a)
     -> Svg
scatterLP f g h spdat lps = forM_ lps fun where
  fun lp = glyph w' sw' sh Nothing (Just col') (_lp lp)
   where
    ScatterPointData sh w' sw' col' = modifyScatterPoint f g h spdat lp
   
      

data ScatterPointData a = ScatterPointData
  {
    spGlyphShape :: GlyphShape_
  , spSize :: a
  , spStrokeWidth :: a
  , spColour :: C.Colour Double
  } deriving (Eq, Show)


modifyScatterPoint
  :: (a -> b -> c)
     -> (a -> b -> c)
     -> (a -> C.Colour Double -> C.Colour Double)
     -> ScatterPointData b
     -> LabeledPoint a d
     -> ScatterPointData c
modifyScatterPoint f g h (ScatterPointData glsh sz w col) lp =
  ScatterPointData glsh (f lab sz) (g lab w) (h lab col)
  where
    lab = _lplabel lp



data GlyphShape_ = Square | Circle | Cross | Plus deriving (Eq, Show, Enum)

-- | Scatterplot glyph shapes
glyph
  :: (Show a, RealFrac a) =>
     a
     -> a
     -> GlyphShape_
     -> Maybe (C.Colour Double)
     -> Maybe (C.Colour Double)
     -> Point a
     -> Svg
glyph w sw Square scol fcol p = squareCentered w sw scol fcol p
glyph w sw Circle scol fcol p = circle w sw scol fcol p
glyph w sw Cross _ fcol p = crossGlyph w sw (fromMaybe C.black fcol) p
glyph w sw Plus _ fcol p = plusGlyph w sw (fromMaybe C.black fcol) p


-- | Utility function for cycling glyph colours and shapes (i.e. unique combinations of these make it easy to tell different datasets apart)
cycleGlyphCols :: (CP.ColorCat, Int) -> Int -> [(CP.Kolor, GlyphShape_)]
cycleGlyphCols (pal, n) nsets = take nsets $ zip (cycle $ CP.brewerSet pal n ) (cycle [Square, Circle ..])





