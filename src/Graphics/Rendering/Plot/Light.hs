{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Graphics.Rendering.Plot.Light
-- Copyright   :  Marco Zocca 2017
-- License     :  BSD3
-- Maintainer  :  Marco Zocca <zocca marco gmail>
--
-- `plot-light` provides functionality for rendering vector
-- graphics in SVG format. It is geared in particular towards scientific plotting, and it is termed "light" because it only requires a few common Haskell dependencies and no external libraries.
--
-- == Usage
--
-- To use this project you just need @import Graphics.Rendering.Plot.Light@. If GHC complains of name clashes you can import the module in "qualified" form.
--
-- If you wish to try out the examples in this page, you will need to have these additional import statements:
--
-- @import Text.Blaze.Svg.Renderer.String (renderSvg)@
-- 
-- @import qualified Data.Colour.Names as C@

module Graphics.Rendering.Plot.Light (
  -- * Plot elements
  -- ** Geometrical primitives
  rect, rectCentered, circle, line, text, polyline, filledPolyline,
  -- ** Composite plot elements
  filledBand, candlestick,
  -- ** Plot utilities
  toPlot, FigureData(..),
  -- ** Element attributes
  LineStroke_(..), StrokeLineJoin_(..), TextAnchor_(..),
  -- ** SVG utilities
  svgHeader, translateSvg,
  -- * Types
  Frame(..), Point(..), LabeledPoint(..), labelPoint, Axis(..),
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
  movePoint, moveLabeledPointV2, (-.), toSvgFrame, toSvgFrameLP, pointRange,
  -- ** Operations on vectors
  frameToFrame, 
  -- ** Operations on frames
  frameFromPoints, mkFrame, mkFrameOrigin, width, height,
  -- ** Typeclasses
  AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..)
  ) where

-- import qualified Data.Text as T

-- import Text.Blaze.Svg
-- import Text.Blaze.Svg.Renderer.String (renderSvg)

-- import qualified Data.Colour as C
-- import qualified Data.Colour.Names as C
-- import qualified Data.Colour.SRGB as C

import Graphics.Rendering.Plot.Light.Internal










