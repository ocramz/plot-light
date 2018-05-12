module Graphics.Rendering.Plot.Light.PlotTypes.Scatter where

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.Internal.Utils

import Data.Fixed
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Text.Blaze.Svg
import qualified Data.Colour as C
-- import qualified Data.Colour.Palette.BrewerSet as CP
import qualified Data.Colour.Names as C
import qualified Data.Text as T


-- | Scatter plot
--
-- Every point in the plot has the same parameters, as declared in the `ScatterPointData` record
scatter :: (Foldable t, Show a, RealFrac a) =>
           ScatterPointData a
        -> t (Point a)
        -> Svg
scatter (ScatterPointData glshape w sw fcol alpha) ps = 
  forM_ ps $ glyph w sw glshape fcol alpha


-- | Parametric scatter plot
--
-- The parameters of every point in the scatter plot are modulated according to the label, using the three functions.
--
-- This can be used to produce rich infographics, in which e.g. the colour and size of the glyphs carry additional information.
scatterLP
  :: (Foldable t, RealFrac a, Show a) =>
     (l -> b -> a)            -- ^ Modifies the glyph size
     -> (l -> b -> a)         -- ^ Modifies the glyph stroke width
     -> (l -> C.Colour Double -> C.Colour Double)  -- ^ Modifies the glyph colour
     -> (l -> b -> a)
     -> ScatterPointData b    -- ^ Glyph style defaults
     -> t (LabeledPoint l a) -- ^ Data
     -> Svg
scatterLP f g h i spdat lps = forM_ lps (scatterLP1 f g h i spdat)


scatterLP1
  :: (Show a, RealFrac a) =>
     (l -> b -> a)            -- ^ Modifies the glyph size
     -> (l -> b -> a)         -- ^ Modifies the glyph stroke width
     -> (l -> C.Colour Double -> C.Colour Double)  -- ^ Modifies the glyph colour
     -> (l -> b -> a)        -- ^ Modifies the glyph opacity
     -> ScatterPointData b   
     -> LabeledPoint l a
     -> Svg
scatterLP1 f g h i spdat lp = glyph w' sw' sh col' a' (_lp lp)
 where
    ScatterPointData sh w' sw' col' a' = modifyScatterPoint f g h i spdat lp



scatterLPBar
  :: (RealFrac t, Enum t, RealFrac b, Show b) =>
     FigureData b
     -> b                 -- ^ Legend width
     -> t                 -- ^ Data value lower bound
     -> t                 -- ^ Data value upper bound
     -> Int               -- ^ Number of legend entries
     -> LegendPosition_   -- ^ Legend position in the figure
     -> b                 -- ^ Legend length
     -> (t -> b -> b)     -- ^ Modifies the glyph size
     -> (t -> b -> b)     -- ^ Modifies the glyph stroke width
     -> (t -> C.Colour Double -> C.Colour Double) -- ^ Modifies the glyph colour
     -> (t -> b -> b)
     -> ScatterPointData b    -- ^ Glyph style defaults
     -> Svg
scatterLPBar fdat w vmin vmax n legpos legh f g h i spdat = legendBar fdat w vmin vmax n legpos legh fun where
  wglyph = spSize spdat
  fun _ _ _ _ _ lp@(LabeledPoint p val) = do
    scatterLP1 f g h i spdat lp
    text 0 (figLabelFontSize fdat) C.black TAStart (T.pack $ show (rr val :: Fixed E3))   (V2 (2 * wglyph) (0.5 * wglyph)) p


  
      
-- | Parameters for a scatterplot glyph
data ScatterPointData a = ScatterPointData
  {
    spGlyphShape :: GlyphShape_
  , spSize :: a
  , spStrokeWidth :: a
  , spColour :: C.Colour Double
  , spAlpha :: a
  } deriving (Eq, Show)


modifyScatterPoint ::
        (l -> b -> c)                             -- ^ Modifies glyph size
     -> (l -> b -> c)                             -- ^ Modifies glyph stroke width
     -> (l -> C.Colour Double -> C.Colour Double) -- ^ Modifies glyph colour
     -> (l -> b -> c)                             -- ^ Modifies glyph opacity
     -> ScatterPointData b
     -> LabeledPoint l d
     -> ScatterPointData c
modifyScatterPoint f g h i (ScatterPointData glsh sz w col alpha) lp =
  ScatterPointData glsh (f lab sz) (g lab w) (h lab col) (i lab alpha)
  where
    lab = _lplabel lp


-- | Glyph shape
data GlyphShape_ = Square | Circle | Cross | Plus deriving (Eq, Show, Enum)

-- | Scatterplot glyph shapes
glyph :: (Show a, RealFrac a) =>
         a               -- ^ Width
      -> a               -- ^ Stroke width
      -> GlyphShape_     -- ^ Glyph shape
      -> C.Colour Double -- ^ Glyph colour
      -> a               -- ^ Opacity
      -> Point a         -- ^ Position
      -> Svg
glyph w sw sh col alpha p =
  let cf = shapeColNoBorder col alpha
  in 
    case sh of
      Square -> squareCentered w cf p
      Circle -> circle w cf p
      Cross -> crossGlyph w sw col p
      Plus -> plusGlyph w sw col p
        


-- -- | Utility function for cycling glyph colours and shapes (i.e. unique combinations of these make it easy to tell different datasets apart)
-- cycleGlyphCols :: (CP.ColorCat, Int) -> Int -> [(CP.Kolor, GlyphShape_)]
-- cycleGlyphCols (pal, n) nsets = take nsets $ zip (cycle $ CP.brewerSet pal n ) (cycle [Square, Circle ..])





