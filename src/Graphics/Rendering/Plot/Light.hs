{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Graphics.Rendering.Plot.Light
-- Copyright   :  Marco Zocca 2017
-- License     :  BSD3
-- Maintainer  :  Marco Zocca <zocca marco gmail>
--
-- `plot-light` provides functionality for rendering vector
-- graphics as SVG. It is geared in particular towards scientific plotting,
-- and it is termed "light" because it only requires native Haskell dependencies.
--
-- It builds upon `blaze-svg` by adding type-safe combinators,
-- geometry primitives and functionaliity
--
-- To use this project you just need to import this module qualified (to avoid name clashes with any other modules you might have loaded on the side), for example as follows :
--
-- @import Graphics.Rendering.Plot.Light as P@

module Graphics.Rendering.Plot.Light (
  -- * Geometry elements
  rectCentered, line, axis, text, polyline,
  -- * Types
  FigureData(..), Point(..), LabeledPoint(..), Axis(..),
  mkFigureData, figure,
  -- * Geometry
  -- ** Vectors
  V2(..),
  -- ** Matrices
  Mat2(..), DiagMat2(..), diagMat2,
  -- ** Typeclasses
  AdditiveGroup(..), VectorSpace(..), Hermitian(..), LinearMap(..), MultiplicativeSemigroup(..), MatrixGroup(..),
  norm2, normalize2,
  mkV2fromEndpoints, v2fromPoint, origin, movePoint, moveLabeledPointV2, fromUnitSquare, toUnitSquare, e1, e2) where

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










