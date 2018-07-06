module Graphics.Rendering.Plot.Light.PlotTypes.Plot1D
  -- (
  --   plotFun, Plot1DOptions(..)
  -- , plotf, plotf'
  -- , Plot1DDomain(..)
  -- , plot, plot'
  -- )
  where

import Graphics.Rendering.Plot.Light.Internal
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Text.Blaze.Svg

-- -- | Graph a 1D function
-- plotFun :: Functor f => (t -> t) -> f (Point t) -> f (Point t)
-- plotFun f = fmap f' where
--   f' (Point x _) = Point x (f x)

-- | Plot the graph of a 1D function
-- plotf :: (Show a, RealFrac a) => (a -> a) -> Svg
-- plotf = plotf' plot1dDomainDefaults

-- plotf' :: (Show a, RealFrac a) => Plot1DDomain a -> (a -> a) -> Svg
-- plotf' (Plot1DDomain n x1 x2) f = plot $ plotFun f ps where
--   ps = pointRange n (Point x1 0) (Point x2 0)

-- plot :: (Foldable t, Show a, RealFrac a) => t (Point a) -> Svg
-- plot = plot' plot1DDefaults

-- plot' :: (Foldable t, Show a, RealFrac a) =>
--          Plot1DOptions a
--       -> t (Point a)
--       -> Svg
plot' (Plot1DOptions sw ty sjt sc) = polyline sw ty sjt sc 

data Plot1DOptions a = Plot1DOptions {
    p1oWidth :: a             -- ^ Stroke width
  , p1oStrokeType :: LineStroke_ a   -- ^ Stroke type 
  , p1oStrokeJoinType :: StrokeLineJoin_ -- ^ Stroke join type 
  , p1oStrokeColour :: C.Colour Double -- ^ Stroke colour
                                     } deriving (Eq, Show)

plot1DDefaults :: RealFrac a => Plot1DOptions a
plot1DDefaults = Plot1DOptions 3 Continuous Round C.blue

data Plot1DDomain a = Plot1DDomain {
    p1dNPoints :: Int
  , p1dPoint1 :: a
  , p1dPoint2 :: a
                                   } deriving (Eq, Show)

plot1dDomainDefaults :: RealFrac a => Plot1DDomain a
plot1dDomainDefaults = Plot1DDomain 100 (-10) 10
