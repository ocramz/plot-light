module Graphics.Rendering.Plot.Light.PlotTypes.Heatmap (heatmap, heatmap', plotFun2) where

import Data.Scientific (Scientific, toRealFloat)
import Graphics.Rendering.Plot.Light.Internal

import Text.Blaze.Svg

import Control.Arrow ((***), (&&&))
import Control.Monad (forM_)

-- import qualified Data.Colour.Names as C
import qualified Data.Colour as C
import Data.Colour.Palette.BrewerSet
-- import Text.Blaze.Svg


-- | A 2D heatmap plot
--
-- `heatmap` assumes the input data corresponds to evenly sampled values of a scalar-valued field, and it maps the data values onto the provided `palette` (which can be created e.g. with `brewerSet`).
heatmap
  :: FigureData Rational   -- ^ Figure data
     -> [C.Colour Double]  -- ^ Colour palette
     -> [[Scientific]]     -- ^ Data
     -> Svg
heatmap fdat palette d = do
  let (nh, nw, vmin, vmax, d') = prepData d
      w = figFWidth fdat / nw
      h = figFHeight fdat / nh
      from = Frame (Point 0 0) (Point 1 1)
      to = frameFromFigData fdat
  forM_ d' (mkPixel palette w h vmin vmax . toFigFrame from to) 

heatmap'
  :: (Foldable f, Functor f, Show a, RealFrac a, RealFrac t) =>
     FigureData a
     -> [C.Colour Double]
     -> Frame a
     -> a
     -> a
     -> f (LabeledPoint t a)
     -> Svg
heatmap' fdat palette from nw nh lp = do
  let
    w = figFWidth fdat / nw
    h = figFHeight fdat / nh
    -- from = Frame (Point 0 0) (Point 1 1)
    -- from = frameFromPoints $ _lp <$> lp
    to = frameFromFigData fdat
    (vmin, vmax) = (minimum &&& maximum) (_lplabel <$> lp)
  forM_ lp (mkPixel' palette w h vmin vmax . moveLabeledPointBwFrames from to False False)

  


toFigFrame
  :: Fractional a =>
     Frame a -> Frame a -> LabeledPoint l Rational -> LabeledPoint l a
toFigFrame from to = moveLabeledPointBwFrames from to False False . fromRationalLP

fromRationalLP :: Fractional a => LabeledPoint l Rational -> LabeledPoint l a
fromRationalLP (LabeledPoint (Point x y) l) = LabeledPoint (Point (fromRational x) (fromRational y)) l


mkPixel
  :: (Show a, RealFrac a) =>
     [C.Colour Double]
     -> a
     -> a
     -> Scientific
     -> Scientific
     -> LabeledPoint Scientific a
     -> Svg
mkPixel palette w h vmin vmax (LabeledPoint p l) = rect w h 0 Nothing (Just col) p where
  col = pickColor palette (toFloat vmin) (toFloat vmax) (toFloat l)

mkPixel'
  :: (Show a, RealFrac a, RealFrac t) =>
     [C.Colour Double] -> a -> a -> t -> t -> LabeledPoint t a -> Svg
mkPixel' palette w h vmin vmax (LabeledPoint p l) = rect w h 0 Nothing (Just col) p where
  col = pickColor palette vmin vmax l
  

pickColor :: RealFrac t => [C.Colour Double] -> t -> t -> t -> C.Colour Double
pickColor palette xmin xmax x = palette !! i
  where
    i = floor (x01 * fromIntegral (nColors - 1))
    x01 = (x-xmin)/(xmax - xmin)
    nColors = length palette


colorBar palette xmin xmax n =
  pickColor palette xmin xmax <$> subdivSegment xmin xmax n


-- | `prepData d` assumes the input lists correspond to evenly sampled values of a scalar-valued field.
--
-- The function extracts the pixel mesh size, the data ranges and places the data points within the unit square [0,1] x [0,1]
prepData ::
  (Ord t, Fractional a, Enum a) =>
     [[t]]  -- ^ Data
  -> (a, a, t, t, [LabeledPoint t a])  -- ^ (# of pixel rows, # of pixel columns, data minimum, data maximum, data points)
prepData ll = (nh, nw, valMin, valMax, d')
  where
    nh = fromIntegral $ length ll
    nw = fromIntegral $ length (head ll)
    -- nh = toRational $ length ll
    -- nw = toRational $ length (head ll)    
    d' = toUnitFramedLP nw nh <$> toCoord ll
    valMin = minimum $ _lplabel <$> d'
    valMax = maximum $ _lplabel <$> d'
    
   

toCoord :: (Num i, Enum i) => [[c]] -> [(i, i, c)]
toCoord ll = concat $ reverse $ go 0 ll [] where
  go i (x:xs) acc = go (i + 1) xs $ zip3 [0 ..] (repeat i) x : acc
  go _ [] acc = acc

toUnitFramedLP :: (Fractional t) =>
      t -> t -> (t, t, l) -> LabeledPoint l t
toUnitFramedLP w h (i, j, x) = LabeledPoint p x
  where p = Point (i/h) (j/w)




plotFun2
  :: Functor f =>
     (t -> t -> l) -> f (Point t) -> f (LabeledPoint l t)
plotFun2 f = fmap f' where
  f' p@(Point x y) = LabeledPoint p (f x y)



