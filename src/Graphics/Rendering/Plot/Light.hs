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
-- To use this project you just need to import this module. If GHC complains with name clashes you can import the module in "qualified form".
--
-- @import Graphics.Rendering.Plot.Light@
--
-- If you wish to try out the examples in this page, you will need to have these additional import statements:
--
-- @import Text.Blaze.Svg.Renderer.String (renderSvg)@
-- 
-- @import qualified Data.Colour.Names as C@

module Graphics.Rendering.Plot.Light (
  -- * Graphical elements
  rectCentered, circle, line, axis, text, polyline,
  -- ** Element attributes
  LineStroke_(..), StrokeLineJoin_(..), 
  -- ** SVG utilities
  svgHeader,
  -- * Types
  Frame(..), Point(..), LabeledPoint(..), Axis(..),
  -- * Geometry
  -- ** Vectors
  V2(..),
  -- ** Matrices
  Mat2(..), DiagMat2(..), diagMat2,
  -- ** Primitive elements
  origin, e1, e2,
  -- ** Vector operations 
  norm2, normalize2,
  -- ** Vector construction
  v2fromEndpoints, v2fromPoint,
  -- ** Operations on points
  movePoint, moveLabeledPointV2, fromUnitSquare, toUnitSquare,
  -- ** Typeclasses
  AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..), Eps(..)
  ) where

-- import Data.Foldable
import qualified Data.Text as T

import Text.Blaze.Svg
-- import Text.Blaze.Svg11 ((!), mkPath, rotate, translate, l, m)
-- import qualified Text.Blaze.Svg11 as S
-- import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import Graphics.Rendering.Plot.Light.Internal










