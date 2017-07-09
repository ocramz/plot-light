module Graphics.Rendering.Plot.Light.Internal.Types (FigureData(..), Point(..), LabeledPoint(..)) where


data FigureData a =
  FigData {
    _width :: a
  , _height :: a
  , _xmin :: a
  , _xmax :: a
  , _ymin :: a
  , _ymax :: a
      } deriving (Eq, Show)


-- | A `Point` defines a point in R2
data Point c a = Point { _px :: c, _py :: c, _p :: a} deriving (Eq, Show)

-- | A `LabeledPoint` carries the information of where a point should be plotted, what label should it carry (e.g. for labelling the axes) and its function value 
data LabeledPoint c l a =
  LabeledPoint { _lp :: Point c a, _lplabel :: l } deriving (Eq, Show)

-- data P1 a = P1 a
-- data P2 a = P2 a a





