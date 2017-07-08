{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Plot.Light where

-- import Data.Foldable
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Blaze.Svg
import Text.Blaze.Svg11 ((!), mkPath, rotate, translate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

-- data BoundingBox a = BBox { _x0 :: a, _y0 :: a, _width :: a, _height :: a} deriving (Eq, Show)











