{-# language TypeFamilies, DeriveFunctor, FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import Data.Bifunctor
import Data.Bifunctor.Pair
import qualified Data.Foldable as F (toList)
import qualified Data.IntMap as IM

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Text.Blaze.Svg
import Text.Blaze.Svg.Renderer.String (renderSvg)



newtype E a = E { unE :: Either (ShC a) (ShNC a) } deriving (Eq, Show)

bimapE :: (a -> b) -> (a -> b) -> E a -> E b
bimapE f g (E ei) = E (bimap (f <$>) (g <$>) ei)

firstE :: (b -> b) -> E b -> E b
firstE f = bimapE f id

secondE :: (b -> b) -> E b -> E b
secondE g = bimapE id g

bothE :: (a -> b) -> E a -> E b
bothE f = bimapE f f

-- | Extract
getE :: (ShC a -> x) -> (ShNC a -> x) -> E a -> x
getE f g ee = either f g $ unE ee



-- | Shapes with centered anchor
data ShC a =
    C a
  | Line a a
  | Gly a 
  | PolyL [a] deriving (Eq, Show, Functor)

-- | Shapes with non-centered anchor
data ShNC a =
    RecBL a -- bottom-left corner
  | RecBC a -- bottom-center
  deriving (Eq, Show, Functor)

mkC :: ShC a -> E a
mkC = E . Left

mkNC :: ShNC a -> E a
mkNC = E . Right


-- | derived combinators

-- transformE r = bimapE f g where
--   f x = (r *) x
--   g x = (r +) x 



mkRecBL :: a -> b -> E (Pair a b)
mkRecBL vd v = mkNC $ RecBL (P vd v) 


-- | 

repositionE :: (Mix2 p, Bifunctor p, Fractional a) =>
               Frame (V2 a)
            -> Frame (V2 a)
            -> E (p (V2 a) (V2 a)) -> E (p (V2 a) (V2 a))
repositionE from to = biasE . frameToFrameBE from to 

-- | Vertical bias applied only to NC shapes (i.e. via `secondE`)
biasE :: (Mix2 p, Num a) => E (p (V2 a) (V2 a)) -> E (p (V2 a) (V2 a))
biasE x = secondE (mix2r fbias) x where
  fbias vd v = v ^-^ fromCartesian 0 (_vy vd)

frameToFrameBE :: (Bifunctor p, MatrixGroup (DiagMat2 a) b, Fractional a) =>
     Frame (V2 a) -> Frame (V2 a) -> E (p b (V2 a)) -> E (p b (V2 a))
frameToFrameBE from to = toFrameBE to . bothE (second flipUD) .fromFrameBE from
  where
    flipUD (V2 vx vy) = mkV2 vx (1 - vy)  
    

fromFrameBE :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
     Frame (V2 a) -> E (p b (V2 a)) -> E (p b (V2 a))
fromFrameBE from = bothE (bimap f g)
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)        
    

toFrameBE :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
     Frame (V2 a) -> E (p b (V2 a)) -> E (p b (V2 a))
toFrameBE to = bothE (bimap f g)
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto



-- | -- --










-- | =============
-- | A DSL for geometrical shapes

data Shp p vd v =
    Circle (ShapeCol p) vd v  -- ^ Circle
  | RectBL (ShapeCol p) vd v  -- ^ Rectangle, anchored bottom-left
  | RectC (ShapeCol p) vd v  -- ^ Rectangle, anchored center
  deriving (Eq, Show)

instance Bifunctor (Shp p) where
  bimap f g sh = case sh of
    Circle k vd v -> Circle k (f vd) (g v)
    RectBL k vd v -> RectBL k (f vd) (g v)
    RectC k vd v -> RectC k (f vd) (g v)    

instance Mix2 (Shp p) where
  mix2 f g sh = case sh of
    Circle k vd v -> Circle k (f vd v) (g vd v)
    RectBL k vd v -> RectBL k (f vd v) (g vd v) 
    RectC k vd v -> RectC k (f vd v) (g vd v)   

renderShape :: (Floating a, Real a) => Shp a (V2 a) (V2 a) -> Svg
renderShape sh = case sh of
  Circle col vd v -> circle r col v where r = norm2 vd
  RectBL col vd v -> rect w h col v where (w, h) = _vxy vd
  -- RectC col vd v -> rect w h col v where (w, h) = _vxy vd
  



reposition :: (Foldable f, Ord a, Functor f, Fractional a) =>
     Frame (V2 a) -> f (Shp p (V2 a) (V2 a)) -> f (Shp p (V2 a) (V2 a))
reposition to shs = reposition1 from to <$> shs where
  from = wrappingFrame shs

reposition1 :: Fractional a =>
               Frame (V2 a)
            -> Frame (V2 a)
            -> Shp p (V2 a) (V2 a)
            -> Shp p (V2 a) (V2 a)
reposition1 from to = bias . frameToFrameB from to 


bias :: Num a => Shp p (V2 a) (V2 a) -> Shp p (V2 a) (V2 a)
bias sh = case sh of
  r@RectBL{} -> mix2r fbias r where
    fbias vd v = v ^-^ fromCartesian 0 (_vy vd)
  x@_ -> x



wrappingFrame :: (Foldable t, AdditiveGroup v, Ord v) => t (Shp p v v) -> Frame v
wrappingFrame shs = foldr insf fzero ssh where
  (sh:ssh) = F.toList shs
  fzero = mkFrameShp sh
  insf s acc = mkFrameShp s `mappend` acc

mkFrameShp :: AdditiveGroup v => Shp p v v -> Frame v
mkFrameShp s = case s of
    Circle _ vd v -> mkFrame v (v ^+^ vd)
    RectBL _ vd v -> mkFrame v (v ^+^ vd)
    RectC _ vd v  -> mkFrame v (v ^+^ vd)    

  
-- | Given :
--
-- * a starting frame (in the screen reference)
-- * a destination frame (in the SVG reference)
-- * a 'Shape' whose anchoring point is assumed to be bound by the starting frame
--
-- compose the affine transformations required to move the 'Shape' from starting to destination frame.
--
-- NB : this should be the /only/ function dedicated to transforming point coordinates
frameToFrameB :: (Bifunctor p, MatrixGroup (DiagMat2 a) v, Fractional a) =>
     Frame (V2 a) -> Frame (V2 a) -> p v (V2 a) -> p v (V2 a)
frameToFrameB from to = toFrameBimap to . second flipUD . fromFrameBimap from where
  flipUD (V2 vx vy) = mkV2 vx (1 - vy)  

fromFrameBimap :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
     Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
fromFrameBimap from = bimap f g
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)    

toFrameBimap :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
     Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
toFrameBimap to = bimap f g
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto









-- | example smart constructors

-- -- mkC :: Num a => a -> ShapeCol p -> v -> Shp p (V2 a) v
-- mkC r col v = Circle col vd v where vd = r .* e1

-- -- mkR :: Num a => a -> a -> ShapeCol p -> v -> Shp p (V2 a) v
-- mkR w h col v = RectBL col vd v where vd = fromCartesian w h

-- -- mkRBC w h col v = RectBL col vs v where
-- --   vs = fromCartesian (w/2) h
-- --   v' = v ^-^ fromCartesian 0 (w/2)

-- mkReC w h col v = RectC col vs v' where
--   vs = 0.5 .* fromCartesian w h  
--   v' = v ^-^ (0.5 .* vs)  
  
-- -- mkReC w h col v = R col vs v' where
-- --   vs = fromCartesian w h
-- --   v' = v ^-^ (0.5 .* vs)  

-- -- mkSqrC :: Fractional a => a -> ShapeCol p -> V2 a -> Shp p (V2 a) (V2 a)
-- -- mkSqrC w col v = mkReC w w col v  

-- c4 = mkC 1 (shapeColNoBorder C.blue 0.6) (mkV2 30 15 )
-- c3 = mkC 1 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- -- r0 = mkR 5 5 (shapeColNoBorder C.red 1) (mkV2 20 20)
-- -- r1 = mkR 5 5 (shapeColNoBorder C.blue 1) (mkV2 0 0)

-- rectb w h x y = mkR w h (shapeColNoBorder C.red 1) (mkV2 x y)
-- r21 = rectb 5 5 0 0
-- r22 = rectb 5 10 10 0
-- r23 = rectb 5 2 20 0
-- r24 = rectb 5 15 30 0
-- -- -- shs = [r0, r1, c3]
-- -- -- shs = [r0, r1]

-- shs = [r21, r22, r23, r24, c3, c4]









-- -- render0 :: (Foldable t, Floating a, Real a, Functor t) =>
-- --            Frame (V2 a)
-- --         -> t (Shp a (V2 a) (V2 a))
-- --         -> Svg
-- render0 to shs = renderShape `mapM_` reposition to shs


-- test0 =
--   do
--   let
--     figdata = figureDataDefault
--     to = frameFromFigData figdata
--     (rout, rin) = rectsFigData figdata 
--     svg_t = svgHeader' figdata $ do
--       render0 to shs
--       renderShape rout
--       renderShape rin
--   T.writeFile "examples/ex_dsl5.svg" $ T.pack $ renderSvg svg_t


-- -- | Rectangles based on the inner and outer frames of the drawable canvas
-- -- rectsFigData :: (Num p, Fractional a) => FigureData a -> (Sh p (V2 a), Sh p (V2 a))
-- rectsFigData fd = (rOut, rIn)
--   where
--     col = shapeColNoFill C.black 1 1
--     frIn = frameFromFigData fd
--     pc = midPoint (_fpmin frIn) (_fpmax frIn)
--     rIn = mkReC (width frIn) (height frIn) col pc 
--     rOut = mkReC (figWidth fd) (figHeight fd) col pc





-- | Some data structs for plotting options

-- | Glyph shape for scatter plots
data GlyphShape_ =
  GlyphPlus | GlyphCross | GlyphCircle | GlyphSquare deriving (Eq, Show)


-- -- -- | Scatterplot glyph shapes
-- -- glyph :: (Show a, RealFrac a) =>
-- --          a               -- ^ Width
-- --       -> a               -- ^ Stroke width
-- --       -> GlyphShape_     -- ^ Glyph shape
-- --       -> C.Colour Double -- ^ Glyph colour
 -- --       -> a               -- ^ Opacity
-- --       -> Point a         -- ^ Position
-- --       -> Svg
-- glyph w sw sh col alpha p =
--   let cf = shapeColNoBorder col alpha
--   in 
--     case sh of
--       GlyphSquare -> squareCentered w cf p
--       GlyphCircle -> circle w cf p
--       GlyphCross -> crossGlyph w sw col p
--       GlyphPlus -> plusGlyph w sw col p

      

-- data PlotOptions p a =
--     LinePlot (LineOptions p) StrokeLineJoin_ a
--   | ScatterPlot Glyph_ (ShapeCol p) a
--   deriving (Eq, Show)


data AxisRange a = AxisRange {aoMin :: a, aoMax :: a}
  deriving (Eq, Show)

data Plot2d a =
  Plot2d (Frame a) (AxisRange a) (AxisRange a)
  deriving (Eq, Show)


data Plot =
    LinePlot
  | Histogram
  deriving (Eq, Show)









-- | =======
-- | HasFrame typeclass, a generic way to create a bounding Frame from something (e.g. a shape) that has a spatial extension

class HasFrame m where
  type FrameTy m :: *
  frame :: m -> Frame (FrameTy m)

instance HasFrame (V2 a) where
  type FrameTy (V2 a) = V2 a
  frame = frameDirac

instance HasFrame (Frame a) where
  type FrameTy (Frame a) = a
  frame = id

-- instance AdditiveGroup v => HasFrame (Shp p v v) where
--   type FrameTy (Shp p v v) = v
--   frame = mkFrameShp


-- -- a generic `wrappingFrame`
-- -- wrappingFrameG :: (Foldable t, HasFrame m, Ord (FrameTy m), Monoid (FrameTy m)) =>
-- --      t m -> Frame (FrameTy m)
-- wrappingFrameG shs = foldr insf fzero ssh where
--   (sh:ssh) = F.toList shs
--   fzero = frame sh
--   insf s acc = frame s `mappend` acc


-- | A thing of type 'sh' in a 'Frame'
-- 
-- | the Frame parameter denotes the destination frame
--
-- NB : we can put more than one shape in a Frame
data Wrt r a sh = Wrt r (Frame a) sh deriving (Eq, Show, Functor)

-- -- instance Semigroup (Wrt r a sh) where
-- -- instance Monoid (Wrt r a sh) where 



-- | Screen reference system (origin is bottom-left screen corner)
data Screen = Screen deriving (Show)
-- | SVG reference system (origin is top-left screen corner)
data SVG = SVG deriving (Show)


-- wrtScreen :: Frame a -> sh -> Wrt Screen a sh
-- wrtScreen = Wrt Screen

wrtSvg :: Frame a -> sh -> Wrt SVG a sh
wrtSvg = Wrt SVG

-- screenToSvg :: Wrt Screen a sh -> Wrt SVG a sh
-- screenToSvg (Wrt Screen fr x) = Wrt SVG fr x                             


-- | A V2 labeled by the reference frame
data V r a = V r (V2 a) deriving (Eq, Show)

mkVScreen :: V2 a -> V Screen a
mkVScreen = V Screen

mkVSvg :: V2 a -> V SVG a
mkVSvg = V SVG






-- | A composite collection type that carries stuff and a 'Frame' that delimits it

-- data Bag a x = Bag a (Frame x) deriving (Eq, Show)

-- instance (Monoid a, Monoid x, Ord x) => Monoid (Bag a x) where
--   mempty = Bag mempty mempty

-- (&) :: (Monoid a, Monoid x, Ord x) => Bag a x -> Bag a x -> Bag a x
-- (Bag f1 fr1) & (Bag f2 fr2) =
--   Bag (f1 `mappend` f2) (fr1 `mappend` fr2)










{- |

YADG : Yet another DSL for graphics

Design :

* add dataset to Plot
* add Plot to WindowState (e.g. side by side plots, inset ... by specifying a RelativeFrame for it)
* compute all viewpanes (i.e. `to` frames)
* compute data transformations from viewpanes

-}

data PlotType = HeatMap | Scatter | TimeSeries 




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

mkRelativeFrame :: (Ord a, Num a) => a -> a -> a -> a -> RelativeFrame a
mkRelativeFrame x y w h
  | all bounded01 [x,y,w,h] = RelFrame x y w h
  | otherwise = RelFrame 0 0 1 1

bounded01 :: (Ord a, Num a) => a -> Bool
bounded01 x = 0 <= x && x <= 1
    

-- data PlotAxis a = PlotAxis
--   { axType :: Axis
--   , axColour :: C.Colour Double
--   , axLabelFontSize :: Int
--   , axRangeMin :: a
--   , axRangeMax :: a
--   , axNTicks :: Int
--   , axTicks :: a -> T.Text  -- `a` is position parameter `0 <= lambda <= 1`
--   }

-- data Plot c a = Plot
--    { plRelativeFrame :: RelativeFrame a
--    , plAxisX :: PlotAxis a
--    , plAxisY :: PlotAxis a
--    , plContents :: c
--    }







-- data Window c a = W
--   { wWidth :: a
--   , wHeight :: a
--   , wState :: IM.IntMap (IM.IntMap (Plot c a))
--   }

-- data Layout c a s =
--   AddPlot (Window c a) (RelativeFrame a) (Window c a -> s)
--   deriving Functor


-- addPlot
--   :: Window c a -> RelativeFrame a -> Free (Layout c a) (Window c a)
-- addPlot w rf = liftF (AddPlot w rf id)


-- * Free


-- liftF :: Functor f => f r -> Free f r
-- liftF x = Free (fmap Pure x)

-- data Free f r = Free (f (Free f r)) | Pure r deriving Functor

-- instance Functor f => Applicative (Free f) where
--   pure = Pure

-- instance (Functor f) => Monad (Free f) where
--     return = pure
--     (Free x) >>= f = Free (fmap (>>= f) x)
--     (Pure r) >>= f = f r
