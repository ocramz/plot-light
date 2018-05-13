module Main where

import Control.Monad (forM_)

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T 
import qualified Data.Text.IO as T (writeFile)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.Internal.Geometry



main = print "hello!"


