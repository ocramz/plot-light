module Graphics.Rendering.Plot.Light.PlotTypes.Plot1D (plotFun) where

import Graphics.Rendering.Plot.Light.Internal

plotFun f = fmap f' where
  f' p@(Point x _) = LabeledPoint p (f x)
