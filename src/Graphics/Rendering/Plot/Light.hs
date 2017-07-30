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
-- To incorporate this library in your projects you just need @import Graphics.Rendering.Plot.Light@. If GHC complains of name clashes you can import the module in "qualified" form.
--
--
-- == Examples
-- 
-- If you wish to try out the examples in this page, you will need to have these additional statements :
--
-- @import Text.Blaze.Svg.Renderer.String (renderSvg)@
-- 
-- @import qualified Data.Colour.Names as C@
-- 
-- === 1. Heatmap plot of generated data
--
-- <<>>
--
-- > import qualified Data.Text.IO as T (readFile, writeFile)
-- > import qualified Data.Text as T
-- >
-- > xPlot = 400
-- > yPlot = 300
-- >
-- > fdat :: FigureData Rational
-- > fdat = FigureData xPlot yPlot 0.1 0.8 0.1 0.9 10
-- >
-- > palette0 = palette [C.red, C.white, C.blue] 15
-- > 
-- > plotFun2ex1 = do
-- >  let 
-- >    p1 = Point (-2) (-2)
-- >    p2 = Point 2 2
-- >    frame = mkFrame p1 p2
-- >    nx = 50 
-- >    ny = 50
-- >    f x y = cos ( pi * theta ) * sin r 
-- >      where
-- >        r = x'**2 + y'**2
-- >        theta = atan2 y' x'
-- >        (x', y') = (fromRational x, fromRational y)
-- >    lps = plotFun2 f $ meshGrid frame nx ny
-- >    pixels = heatmap' fdat palette0 frame nx ny lps
-- >    svg_t = svgHeader xPlot yPlot pixels
-- >  T.writeFile "heatmap.svg" $ T.pack $ renderSvg svg_t
--
-- This example demonstrates how to plot a 2D scalar function and write the output to SVG file.
--
-- First, we define a `Frame` that bounds the rendering canvas. This is discretized in `nx` by `ny` pixels, and the function `f` is computed at the _intersections_ of the mesh
--
-- The function (represented in this case as a list of `LabeledPoint`s, in which the "label" carries the function value) is then mapped onto the given colour palette and drawn to the SVG canvas as a mesh of filled rectangles (Caution: do not exceed resolutions of ~ hundred pixels per side).
--
-- As a last step, the SVG content is wrapped in the appropriate markdown by `svgHeader` and written to file.

module Graphics.Rendering.Plot.Light (
  -- * Plot types
  -- ** Heatmap
  heatmap, heatmap', plotFun2, colourBar, 
  -- * Plot elements
  -- ** Geometrical primitives
  rect, rectCentered, squareCentered, circle, line, text, polyline, filledPolyline, pixel, pixel', 
  -- ** Composite plot elements
  filledBand, candlestick,
  -- ** Plot utilities
  toPlot, FigureData(..),
  -- ** Element attributes
  LineStroke_(..), StrokeLineJoin_(..), TextAnchor_(..),
  -- ** Colour utilities
  blendTwo, palette, pickColour, 
  -- ** SVG utilities
  svgHeader, translateSvg,
  -- * Types
  Frame(..), Point(..), LabeledPoint(..), labelPoint, mapLabel, Axis(..),
  -- * Geometry
  -- ** Vectors
  V2(..),
  -- ** Matrices
  Mat2(..), DiagMat2(..), diagMat2,
  -- ** Primitive elements
  origin, e1, e2,
  -- ** Vector norm operations 
  norm2, normalize2,
  -- ** Vector construction
  v2fromEndpoints, v2fromPoint,
  -- ** Operations on points
  movePoint, moveLabeledPointV2, moveLabeledPointBwFrames, (-.), toSvgFrame, toSvgFrameLP, pointRange,
  -- ** Operations on vectors
  frameToFrame, 
  -- ** Operations on frames
  frameFromPoints, frameFromFigData, mkFrame, mkFrameOrigin, width, height, figFWidth, figFHeight,
  -- ** Typeclasses
  AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..),
  -- ** Helpers
  meshGrid, toFloat, wholeDecimal
  ) where

-- import qualified Data.Text as T

-- import Text.Blaze.Svg
-- import Text.Blaze.Svg.Renderer.String (renderSvg)

-- import qualified Data.Colour as C
-- import qualified Data.Colour.Names as C
-- import qualified Data.Colour.SRGB as C

import Graphics.Rendering.Plot.Light.Internal
import Graphics.Rendering.Plot.Light.PlotTypes









