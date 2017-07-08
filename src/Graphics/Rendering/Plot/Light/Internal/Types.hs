module Graphics.Rendering.Plot.Light.Internal.Types where


data FigureData a d =
  FigData {
    _width :: a
  , _height :: a
  , _xmin :: a
  , _xmax :: a
  , _ymin :: a
  , _ymax :: a
  , _figData :: d
      }

-- | A LabeledPoint carries the information of where a point should be plotted, what label should it carry (e.g. for labelling the axes) and its function value 
data LabeledPoint c l a =
  LabeledPoint { _coord :: c, _label :: l, _value :: a }

data P1 a = P1 a
data P2 a = P2 a a
