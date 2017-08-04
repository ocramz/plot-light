{-# language DeriveFunctor #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import qualified Data.IntMap as IM

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal

import qualified Data.Colour as C
import qualified Data.Text as T


{- |

YADG : Yet another DSL for graphics

Design :

* add dataset to Plot
* add Plot to WindowState (e.g. side by side plots, inset ...)
* compute all viewpanes (i.e. `to` frames)
* compute data transformations from viewpanes

-}



data PlotAxis a = PlotAxis
  { axType :: Axis
  , axColour :: C.Colour Double
  , axLabelFontSize :: Int
  , axRangeMin :: a
  , axRangeMax :: a
  , axNTicks :: Int
  , axTicks :: a -> T.Text  -- `a` is position parameter `0 <= lambda <= 1`
  }

data Plot c a = Plot
   { plRelativeFrame :: RelativeFrame a
   , plAxisX :: PlotAxis a
   , plAxisY :: PlotAxis a
   , plContents :: c
   }

-- | A `RelativeFrame` is given by two set of parameters:
--
-- 0 <= `rfX`, `rfY` <= 1 : normalized coordinates of the anchor point (top-left corner)
-- 0 <= `rfHeight`, `rfWidth` <= 1 : normalized width and height 
data RelativeFrame a = RelFrame
  { rfX :: a
  , rfY :: a
  , rfWidth :: a
  , rfHeight :: a
  } deriving (Eq, Show)


data Window c a = W
  { wWidth :: a
  , wHeight :: a
  , wState :: IM.IntMap (IM.IntMap (Plot c a))
  }

data Layout c a s =
  AddPlot (Window c a) (RelativeFrame a) (Window c a -> s)
  deriving Functor




addPlot
  :: Window c a -> RelativeFrame a -> Free (Layout c a) (Window c a)
addPlot w rf = liftF (AddPlot w rf id)




liftF :: Functor f => f r -> Free f r
liftF x = Free (fmap Pure x)

data Free f r = Free (f (Free f r)) | Pure r deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure

instance (Functor f) => Monad (Free f) where
    return = pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
