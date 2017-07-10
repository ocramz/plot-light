{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Graphics.Rendering.Plot.Light
-- Copyright   :  Marco Zocca 2017
-- License     :  BSD3
-- Maintainer  :  Marco Zocca <zocca marco gmail>
--
-- `plot-light` provides functionality for rendering vector
-- graphics in SVG format. It is geared in particular towards scientific plotting,
-- and it is termed "light" because it only requires few native Haskell dependencies.
--
-- It builds upon `blaze-svg` by adding type-safe combinators,
-- geometry primitives and related functions.
--
-- == Usage
--
-- To use this project you just need to import this module qualified (to avoid name clashes with any other modules you might have loaded on the side), for example as follows :
--
-- @import Graphics.Rendering.Plot.Light as P@
--
-- If you wish to try out the examples in this page, you will need to have `renderSvg` in scope as well:
--
-- @import Text.Blaze.Svg.Renderer.String (renderSvg)@

module Graphics.Rendering.Plot.Light (
  -- * Graphical elements
  rectCentered, line, axis, text, polyline,
  -- * Types
  FigureData(..), Point(..), LabeledPoint(..), Axis(..),
  mkFigureData, svgHeader,
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
  mkV2fromEndpoints, v2fromPoint, 
  -- ** Operations on points
  movePoint, moveLabeledPointV2, fromUnitSquare, toUnitSquare,
  -- ** Typeclasses
  AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..)
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










