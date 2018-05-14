{-# language RecordWildCards #-}
module Graphics.Rendering.Plot.Light.PlotTypes.Heatmap (heatmap, heatmap', plotFun2) where

import Control.Monad.State
import Data.Scientific (Scientific, toRealFloat)
import Graphics.Rendering.Plot.Light.Internal

import Text.Blaze.Svg

import Control.Arrow ((***), (&&&))
import Control.Monad (forM_)


import qualified Data.Colour as C
import qualified Data.Colour.Names as C
-- import Data.Colour.Palette.BrewerSet
-- import Text.Blaze.Svg



data MeshGrid2d a = MeshGrid2d {
    mgFrame :: Frame a
  , mgNx :: Int
  , mgNy :: Int
                           } deriving (Eq, Show)

data Heatmap a = Heatmap {
    hmMesh :: MeshGrid2d a
  , hmPalette :: [C.Colour Double]
  , hmValMin :: a
  , hmValMax :: a
                         } deriving (Eq, Show)

heatmapDefaults :: Num a => Heatmap a
heatmapDefaults = Heatmap mesh pal 0 1
  where
    pal = palette [C.red, C.white, C.blue] 20
    pmin = Point 0 0
    pmax = Point 1 1
    mesh = MeshGrid2d (Frame pmin pmax) 20 20

  




-- | `heatmap` assumes the input data corresponds to evenly sampled values of a scalar-valued field, and it maps the data values onto the provided `palette` (which can be created e.g. with `brewerSet`).
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
  forM_ d' (pixel palette w h vmin vmax . toFigFrame from to) 

-- | `heatmap'` renders one SVG pixel for every `LabeledPoint` supplied as input. The `LabeledPoint`s must be bounded by the `Frame`.
heatmap'
  :: (Foldable f, Functor f, Show a, RealFrac a, RealFrac t) =>
     FigureData a         -- ^ Figure data
     -> [C.Colour Double] -- ^ Colour palette
     -> Frame a           -- ^ Frame containing the data
     -> a                 -- ^ Number of points along x axis
     -> a                 -- ^ " y axis
     -> f (LabeledPoint t a) -- ^ Data
     -> Svg
heatmap' fdat palette from nw nh lp = do
  let
    w = figFWidth fdat / nw
    h = figFHeight fdat / nh
    to = frameFromFigData fdat
    (vmin, vmax) = (minimum &&& maximum) (_lplabel <$> lp)
  forM_ lp (pixel' palette w h vmin vmax . moveLabeledPointBwFrames from to False False)

  


toFigFrame
  :: Fractional a =>
     Frame a -> Frame a -> LabeledPoint l Rational -> LabeledPoint l a
toFigFrame from to = moveLabeledPointBwFrames from to False False . fromRationalLP

fromRationalLP :: Fractional a => LabeledPoint l Rational -> LabeledPoint l a
fromRationalLP (LabeledPoint (Point x y) l) = LabeledPoint (Point (fromRational x) (fromRational y)) l





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



-- | Plot a scalar function `f` of points in the plane (i.e. \(f : \mathbf{R}^2 \rightarrow \mathbf{R}\))
plotFun2
  :: Functor f =>
     (t -> t -> l) -> f (Point t) -> f (LabeledPoint l t)
plotFun2 f = fmap f' where
  f' p@(Point x y) = LabeledPoint p (f x y)




