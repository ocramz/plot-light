module Graphics.Rendering.Plot.Light.PlotTypes.Plot1D (plotFun, Plot1DOptions(..)) where

import Graphics.Rendering.Plot.Light.Internal
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Text.Blaze.Svg

-- | Plot a 1D function
plotFun :: Functor f => (t -> t) -> f (Point t) -> f (Point t)
plotFun f = fmap f' where
  f' (Point x _) = Point x (f x)

plot :: (Foldable t, Show a, RealFrac a) => t (Point a) -> Svg
plot = plot' plot1DDefaults

plot' :: (Foldable t, Show a, RealFrac a) =>
         Plot1DOptions a
      -> t (Point a)
      -> Svg
plot' (Plot1DOptions sw ty sjt sc) dats = polyline sw ty sjt sc dats

data Plot1DOptions a = Plot1DOptions {
    p1oWidth :: a             -- ^ Stroke width
  , p1oStrokeType :: LineStroke_ a   -- ^ Stroke type 
  , p1oStrokeJoinType :: StrokeLineJoin_ -- ^ Stroke join type 
  , p1oStrokeColour :: C.Colour Double -- ^ Stroke colour
                                     } deriving (Eq, Show)

plot1DDefaults :: RealFrac a => Plot1DOptions a
plot1DDefaults = Plot1DOptions 3 Continuous Round C.blue 
