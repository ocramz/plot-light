module Graphics.Rendering.Plot.Light.Internal.Types  where


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
data Point a = Point { _px :: a, _py :: a} deriving (Eq, Show)

mkPoint :: a -> a -> Point a
mkPoint = Point

-- | A `LabeledPoint` carries the information of where a point should be plotted, what label should it carry (e.g. for labelling the axes) and its function value 
data LabeledPoint l a =
  LabeledPoint {
   _lp :: Point a,
   _lplabel :: l } deriving (Eq, Show)


-- | A frame, i.e. a bounding box for objects
data Frame a = Frame {
   _fpmin :: Point a,
   _fpmax :: Point a} deriving (Eq, Show)

xmin, xmax, ymin, ymax :: Frame a -> a
xmin = _px . _fpmin
xmax = _px . _fpmax
ymin = _py . _fpmin
ymax = _py . _fpmax

-- | The `width` is the extent in the `x` direction and `height` is the extent in the `y` direction
width, height :: Num a => Frame a -> a
width f = xmax f - xmin f
height f = ymax f - ymin f
