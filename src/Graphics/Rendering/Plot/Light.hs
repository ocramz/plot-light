{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Graphics.Rendering.Plot.Light
-- Copyright   :  Marco Zocca 2017
-- License     :  BSD3
-- Maintainer  :  Marco Zocca <zocca marco gmail>
--
-- @plot-light@ provides functionality for rendering vector
-- graphics in SVG format. It is geared in particular towards scientific plotting, and it is termed "light" because it only requires a few common Haskell dependencies and no external libraries.
--
-- == Usage
--
-- To incorporate this library in your projects you just need @import Graphics.Rendering.Plot.Light@. If GHC complains of name collisions you must import the module in "qualified" form.
--
--
-- == Examples
-- 
-- If you wish to try out the examples in this page, you will need to create a project that imports @blaze-svg@, @colour@ and @text@, and to have these import statements as well :
--
-- > import Text.Blaze.Svg.Renderer.String (renderSvg) 
-- > import qualified Data.Colour.Names as C
-- > import qualified Data.Text.IO as T (readFile, writeFile)
-- > import qualified Data.Text as T
-- 
-- === 1. Heatmap plot of a 2D function
--
-- <<doc/fig/heatmap.png>>
--
-- This example renders the function
--
-- \[
-- {f(x, y) = \cos(\pi \theta) \sin( \rho^2) }
-- \]
-- where
-- \( \rho^2 = x^2 + y^2 \) and \( \theta = \arctan(y/x) \).
--
--
-- > xPlot = 400
-- > yPlot = 300
-- > fnameOut = "heatmap.svg"
--
-- @
-- fdat = 'FigureData' xPlot yPlot 0.1 0.8 0.1 0.9 10
-- @
-- 
-- @
-- palette0 = 'palette' [C.red, C.white, C.blue] 15
-- @
--
-- @
-- plotFun2ex1 = do
--   let 
--     p1 = Point (-2) (-2)
--     p2 = Point 2 2
--     frame = 'mkFrame' p1 p2
--     nx = 50 
--     ny = 50
--     f x y = cos ( pi * theta ) * sin r 
--       where
--         r = x'**2 + y'**2
--         theta = atan2 y' x'
--         (x', y') = (fromRational x, fromRational y)
--     lps = 'plotFun2' f $ meshGrid frame nx ny
--     labs = map _lplabel lps
--     vmin = minimum labs
--     vmax = maximum labs
--     pixels = 'heatmap'' fdat palette0 frame nx ny lps
--     cbar = 'colourBar' fdat palette0 10 vmin vmax 10 TopRight 100
--     svg_t = 'svgHeader' xPlot yPlot $ do
--        'axes' fdat frame 2 C.black 10 10
--        pixels
--        cbar
-- T.writeFile fnameOut $ T.pack $ renderSvg svg_t
-- @
--
-- This example demonstrates how to plot a 2D scalar function and write the output to SVG file.
--
-- First, we define a `FigureData` object (which holds the SVG figure dimensions and parameters for the white margin around the rendering canvas) and a `palette`.
--
--Afterwards we declare a `Frame` that bounds the rendering canvas using `mkFrame`. This is discretized in `nx` by `ny` pixels with `meshGrid`, and the function `f` is computed at the /intersections/ of the mesh with `plotFun2`.
--
-- The `axes` function adds labeled axes to the figure; the user just needs to specify stroke width and color and how many ticks to display.
--
-- The data to be plotted (represented in this case as a list of `LabeledPoint`s, in which the "label" carries the function value) are then mapped onto the given colour palette and drawn to the SVG canvas as a `heatmap'`, i.e. a mesh of filled rectangles (Caution: do not exceed resolutions of ~ hundred pixels per side).
--
-- Next, we create the legend; in this case this is a `colourBar` element that requires the data bounds `vmin`, `vmax`.
--
-- As a last step, the SVG content is wrapped in the appropriate markdown by `svgHeader` and written to file.
--
--
-- === 2. Scatter plot of 3D data
--
-- <<doc/fig/scatter-2.png>>
--
-- This example shows how to plot a collection of labelled points in the plane. Each sample row is represented by a `LabeledPoint`, in which the label is a scalar quantity.
--
-- The `scatterLP` function renders each data row as a glyph, by modifying a `ScatterPointData` record of default values via four functions that control the glyph size, contour line thickness, colour and opacity. This functionality can be exploited in creative ways to achieve effective infographics.
--
-- > xPlot = 400
-- > yPlot = 300
-- > fnameOut = "scatter.svg"
--
-- @ 
-- fdat = 'FigureData' xPlot yPlot 0.1 0.8 0.1 0.9 10
-- @
--
-- @
-- dats = zipWith 'LabeledPoint' p_ l_ where
--    l_ = [-5, -4 .. ]
--    p_ = zipWith 'Point' [46,30,4,7,73,12,23,90,34,24,5,6,12,3,55,61] [20,35,43,23,20,1,23,8,11,17,25,4,5,26, 30]
-- @
--
-- @
-- spdata = ScatterPointData Plus 3 3 C.black 0.8
-- @
--
-- @
-- main :: IO ()
-- main = do
--    let
--      frameTo = 'frameFromFigData' fdat
--      points = map _lp dats
--      frameFrom = 'frameFromPoints' points
--      labs = map _lplabel dats
--      vmin = minimum labs
--      vmax = maximum labs
--      f l sz = 15 / (1 + exp(- (0.3 * x)) )
--        where x = l + sz
--      g l w = w * (1 + l / (1 + abs l))
--      h l col = C.blend l' C.red col
--        where
--          l' = (l - vmin)/(vmax - vmin)
--      i l alp = alp * ( 1 + l / (1 + abs l))
--      dats' = map ('moveLabeledPointBwFrames' frameFrom frameTo False True) dats
--      svg_t = 'svgHeader' xPlot yPlot $ do
--        'axes' fdat frameFrom 2 C.black 10 10
--        'scatterLP' f g h i spdata dats'
--        'scatterLPBar' fdat 50 vmin vmax 3 TopRight 100 f g h i spdata
--    T.writeFile fnameOut $ T.pack $ renderSvg svg_t
-- @

module Graphics.Rendering.Plot.Light (
  -- * Plot types
  -- ** 1D plot
  plotFun, Plot1DOptions(..),
  -- ** Heatmap
  heatmap, heatmap', plotFun2, colourBar,
  -- ** Scatter
  scatter, scatterLP, scatterLPBar, ScatterPointData(..), GlyphShape_(..),
  -- ** Histogram
  densityD, histogramD,
  -- * Plot elements
  -- ** Geometric primitives
  -- *** Rectangle, square
  rect, rectCentered, rectCenteredMidpointBase, squareCentered,
  -- *** Circle
  circle,
  -- *** Line
  line,
  -- *** Text
  text, TextAnchor_(..), 
  -- *** Polyline
  polyline, filledPolyline, StrokeLineJoin_(..), 
  -- *** Pixel
  pixel, pixel', 
  -- ** Composite plot elements
  filledBand, candlestick, 
  -- ** Plot utilities
  axes, toPlot, FigureData(..),
  -- ** Element attributes
  LineStroke_(..), LegendPosition_(..),
  -- ** Operations on frames
  frameFromPoints, frameFromFigData, mkFrame, mkFrameOrigin, width, height, figFWidth, figFHeight,  
  -- ** Colour utilities
  blendTwo, palette, pickColour,
  -- *** ShapeCol-related
  ShapeCol(..), Col(..), shapeColNoBorder, shapeColNoFill, shapeColBoth,
  -- ** TimeSeries utilities
  fromTick, toTick,
  -- ** SVG utilities
  svgHeader, translateSvg, scaleSvg, toSvgFrame, toSvgFrameLP,
  -- * Geometric types
  -- ** Frame
  Frame(..),
  -- ** Point, LabeledPoint
  Point(..), LabeledPoint(..), labelPoint, mapLabel,
  -- ** Axis
  Axis(..),
  -- * Helpers
  meshGrid,
  -- *** Misc.
  toFloat, wholeDecimal
  ) where

-- import qualified Data.Text as T

-- import Text.Blaze.Svg
-- import Text.Blaze.Svg.Renderer.String (renderSvg)

-- import qualified Data.Colour as C
-- import qualified Data.Colour.Names as C
-- import qualified Data.Colour.SRGB as C

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.PlotTypes









