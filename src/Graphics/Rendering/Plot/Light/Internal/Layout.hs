{-# language TypeFamilies, DeriveFunctor, FlexibleContexts #-}
module Graphics.Rendering.Plot.Light.Internal.Layout where

import Data.Bifunctor
import Data.Bifunctor.Pair
import qualified Data.Foldable as F (toList)
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Control.Monad (ap)

import Graphics.Rendering.Plot.Light.Internal.Geometry
import Graphics.Rendering.Plot.Light.Internal

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Text.Blaze.Svg (Svg)
import Text.Blaze.Svg.Renderer.String (renderSvg)




newtype Compose f g a = Compose { unCompose :: f (g a) }

instance (Functor g, Functor f) => Functor (Compose f g) where
  fmap f = Compose . fmap (fmap f) . unCompose
  -- fmap f (Compose fga) = Compose ((fmap . fmap ) f fga)

instance (Applicative g, Applicative f) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  -- -- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose k = Compose $ (<*>) <$> f <*> k
  -- Compose f <*> Compose k = Compose $ liftedAp <*> k where
  --   liftedAp = fmap (<*>) f 

-- instance Foldable (Compose f g) where
--   foldr = _


-- | ========= 

-- | A shape is :
--
-- * Anchored either at its center or not
-- * Has an anchor point (= position vector) and zero or more size vectors (e.g. a rectangle has only one size vector (i.e. is uniquely defined by its position vector and its size vector), a general N-polygon has N)
newtype Shape a = Shape { unShape :: E (Pair [V2 a] (V2 a)) } deriving (Eq, Show)

mkShape :: E (Pair [V2 a] (V2 a)) -> Shape a
mkShape = Shape

 

-- data PlotTy a =
--     Scatter [a]
--   | PolyLine [a]
--   deriving (Eq, Show)

-- data PlotElem p a =
--    Line a a
--  | Circle (ShapeCol p) a 
--  | RectBL (ShapeCol p) a
--  | RectC (ShapeCol p) a
--  deriving (Eq, Show, Functor)

-- mkRectBL :: Num a => a -> a -> ShapeCol p -> V2 a -> PlotElem p (Shape a)
-- mkRectBL w h col v = RectBL col $ mkShape $ mkNC (mkPair [vd] v) where
--   vd = fromCartesian w h

plotRect :: Real a => ShapeCol a -> E (Pair (V2 a) (V2 a)) -> Svg
plotRect col ee = rect w h col v where
    (vd, v) = getVs ee
    (w, h) = _vxy vd

-- withVs ff f pe = case pe of
--   Line p1 p2 -> ff (f p1) (f p2)

-- -- interpretPE :: Real a => PlotElem a (E (Pair (V2 a) (V2 a))) -> Svg
-- interpretPE sh = case sh of
--   Line p1 p2 -> line v1 v2 sw lst col where
--     v1 = snd $ getVs p1
--     v2 = snd $ getVs p2    
--   Circle col ee -> circle r col v where
--     (vd, v) = getVs ee
--     r = norm2 vd
--   RectC col ee -> plotRect col ee
--   RectBL col ee -> plotRect col ee



 




-- | =======

data GSh vd v  =
    Cir (ExtC vd v)
  | RectBL (ExtNC vd v)
  | Lin (Point v) (Point v)
  | PLyn [(Point v)] deriving (Eq, Show)

interpGSh :: (ExtC vd v -> p)
          -> (ExtNC vd v -> p)
          -> ([Point v] -> p)
          -> GSh vd v
          -> p
interpGSh fc fnc fp sh = case sh of
  Cir ec -> fc ec
  RectBL enc -> fnc enc
  Lin p1 p2 -> fp [p1, p2]
  PLyn ps -> fp ps

newtype Point v = Point v deriving (Eq, Show)
newtype ExtC vd v = ExtC (Pair vd v) deriving (Eq, Show)
newtype ExtNC vd v = ExtNC (Pair vd v) deriving (Eq, Show)



-- instance Bifunctor Sh where
--   bimap f g sh = case sh of
--     Point c -> Point (g c)
--     ExtC vd v -> ExtC (f vd) (g v)
--     ExtNC vd v -> ExtNC (f vd) (g v)

-- -- | Like 'either'
-- -- interpretSh :: (b -> x) -> (a -> b -> x) -> Sh a b -> x
-- -- interpretSh f g sh = case sh of
-- --   Point v     -> f v
-- --   ExtC vd v   -> g vd v
-- --   ExtNC vd v  -> g vd v

-- interpretSh f g sh = case sh of
--   Point v     -> f v
--   ExtC vd v   -> g vd v
--   ExtNC vd v  -> g vd v

-- -- | Like 'mix2r'
-- biasSh :: (vd -> v -> v) -> Sh vd v -> Sh vd v
-- biasSh f sh = case sh of
--   ExtC vd v -> ExtC vd (f vd v)
--   ExtNC vd v -> ExtNC vd (f vd v)
--   c -> c




-- | =======
    


-- | Anchored shapes
-- 
-- | Left  := Centered shapes
-- | Right := Non-centered shapes
newtype E a = E { unE :: Either a a } deriving (Eq, Show)

instance Functor E where
  fmap f (E ei) = E (bimap f f ei)

firstE, secondE :: (a -> a) -> E a -> E a
firstE f (E ei) = E $ first f ei
secondE g (E ei) = E $ second g ei

-- * Extract/interpret
interpretE :: (a -> x) -> E a -> x
interpretE f ee = either f f (unE ee) 

-- * Constructors
mkC :: a -> E a
mkC = E . Left 

mkNC :: a -> E a
mkNC = E . Right 


-- | derived combinators

getVs :: E (Pair a b) -> (a, b)
getVs = interpretE ff where
  ff (P vd v) = (vd, v)






-- type Shape p v = E (p v v)
-- type Shape1 v = E (Pair v v)


-- mkRecBL :: a -> a -> Shape1 a -- E (Pair a a)
-- mkRecBL vd v = mkNC $ RecBL (P vd v) 

-- mkHull :: (AdditiveGroup v, Functor f) => Pair (f v) v -> f v
-- mkHull (P vds v) = f <$> vds where
--   f vd = v ^+^ vd




-- | --

-- -- | Constructs a Frame from a shape, using the second parameter only (position vectors)
-- mkFrameE :: Ord a => E (Pair x a) -> Frame a
-- mkFrameE = interpretE (frameFromPointsWith f) where
--   f (P _ v) = v

-- wrappingFrameE :: (Foldable t, Ord v, Monoid v) => t (E (Pair v1 v)) -> Frame v
-- wrappingFrameE shs = foldr insf fzero ssh where
--   (sh:ssh) = F.toList shs
--   fzero = mkFrameE sh
--   insf s acc = mkFrameE s `mappend` acc




-- | --

-- -- repositionE :: (Foldable f, Ord a, Functor f, Fractional a) =>
-- --                Frame (V2 a)
-- --             -> f (E (Pair (V2 a) (V2 a)))
-- --             -> f (E (Pair (V2 a) (V2 a)))
-- repositionE to shs = reposition1E from to <$> shs where
--   from = wrappingFrameE shs

-- | Reposition a single shape
-- reposition1E :: (Mix2 p, Fractional a) =>
--                 Frame (V2 a)
--              -> Frame (V2 a)
--              -> E (p (V2 a) (V2 a)) -> E (p (V2 a) (V2 a))
-- reposition1E from to = biasE . frameToFrameBE from to

-- | Vertical bias applied only to NC shapes (i.e. via `secondE`)
-- biasE :: (Mix2 p, Num a) => E (p (V2 a) (V2 a)) -> E (p (V2 a) (V2 a))
-- biasE x = secondE (mix2r fbias) x where
--   fbias vd v = v ^-^ fromCartesian 0 (_vy vd)

-- | Modify the position component of a pair using both size and position parameters.
-- This only applies to non-centered shapes (i.e. via `secondE`)
-- biasEWith :: Mix2 p => (x -> a -> a) -> E (p x a) -> E (p x a)
-- biasEWith fbias = secondE (mix2r fbias)

frameToFrameBE from to = toFrameBE to . second flipUD . fromFrameBE from
  where
    flipUD (V2 vx vy) = mkV2 vx (1 - vy)  
    
fromFrameBE :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
               Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
fromFrameBE from = bimap f g
  where
    (mfrom, vfrom) = frameToAffine from
    f v = mfrom <\> v
    g v = mfrom <\> (v ^-^ vfrom)        
    
toFrameBE :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
             Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
toFrameBE to = bimap f g
  where
    (mto, vto) = frameToAffine to
    f v = mto #> v
    g v = (mto #> v) ^+^ vto



-- | -- --










-- | =============
-- | A DSL for geometrical shapes

-- data Shp p vd v =
--     Circle (ShapeCol p) vd v  -- ^ Circle
--   | RectBL (ShapeCol p) vd v  -- ^ Rectangle, anchored bottom-left
--   | RectC (ShapeCol p) vd v  -- ^ Rectangle, anchored center
--   deriving (Eq, Show)

-- instance Bifunctor (Shp p) where
--   bimap f g sh = case sh of
--     Circle k vd v -> Circle k (f vd) (g v)
--     RectBL k vd v -> RectBL k (f vd) (g v)
--     RectC k vd v -> RectC k (f vd) (g v)    

-- instance Mix2 (Shp p) where
--   mix2 f g sh = case sh of
--     Circle k vd v -> Circle k (f vd v) (g vd v)
--     RectBL k vd v -> RectBL k (f vd v) (g vd v) 
--     RectC k vd v -> RectC k (f vd v) (g vd v)   

-- renderShape :: (Floating a, Real a) => Shp a (V2 a) (V2 a) -> Svg
-- renderShape sh = case sh of
--   Circle col vd v -> circle r col v where r = norm2 vd
--   RectBL col vd v -> rect w h col v where (w, h) = _vxy vd
--   -- RectC col vd v -> rect w h col v where (w, h) = _vxy vd
  

-- reposition :: (Foldable f, Ord a, Functor f, Fractional a) =>
--      Frame (V2 a) -> f (Shp p (V2 a) (V2 a)) -> f (Shp p (V2 a) (V2 a))
-- reposition to shs = reposition1 from to <$> shs where
--   from = wrappingFrame shs

-- reposition1 :: Fractional a =>
--                Frame (V2 a)
--             -> Frame (V2 a)
--             -> Shp p (V2 a) (V2 a)
--             -> Shp p (V2 a) (V2 a)
-- reposition1 from to = bias . frameToFrameB from to 


-- bias :: Num a => Shp p (V2 a) (V2 a) -> Shp p (V2 a) (V2 a)
-- bias sh = case sh of
--   r@RectBL{} -> mix2r fbias r where
--     fbias vd v = v ^-^ fromCartesian 0 (_vy vd)
--   x@_ -> x



-- wrappingFrame :: (Foldable t, AdditiveGroup v, Ord v) => t (Shp p v v) -> Frame v
-- wrappingFrame shs = foldr insf fzero ssh where
--   (sh:ssh) = F.toList shs
--   fzero = mkFrameShp sh
--   insf s acc = mkFrameShp s `mappend` acc

-- mkFrameShp :: AdditiveGroup v => Shp p v v -> Frame v
-- mkFrameShp s = case s of
--     Circle _ vd v -> mkFrame v (v ^+^ vd)
--     RectBL _ vd v -> mkFrame v (v ^+^ vd)
--     RectC _ vd v  -> mkFrame v (v ^+^ vd)    

  
-- -- | Given :
-- --
-- -- * a starting frame (in the screen reference)
-- -- * a destination frame (in the SVG reference)
-- -- * a 'Shape' whose anchoring point is assumed to be bound by the starting frame
-- --
-- -- compose the affine transformations required to move the 'Shape' from starting to destination frame.
-- --
-- -- NB : this should be the /only/ function dedicated to transforming point coordinates
-- frameToFrameB :: (Bifunctor p, MatrixGroup (DiagMat2 a) v, Fractional a) =>
--      Frame (V2 a) -> Frame (V2 a) -> p v (V2 a) -> p v (V2 a)
-- frameToFrameB from to = toFrameBimap to . second flipUD . fromFrameBimap from where
--   flipUD (V2 vx vy) = mkV2 vx (1 - vy)  

-- fromFrameBimap :: (MatrixGroup (DiagMat2 a) b, Fractional a, Bifunctor p) =>
--      Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
-- fromFrameBimap from = bimap f g
--   where
--     (mfrom, vfrom) = frameToAffine from
--     f v = mfrom <\> v
--     g v = mfrom <\> (v ^-^ vfrom)    

-- toFrameBimap :: (Num a, LinearMap (DiagMat2 a) b, Bifunctor p) =>
--      Frame (V2 a) -> p b (V2 a) -> p b (V2 a)
-- toFrameBimap to = bimap f g
--   where
--     (mto, vto) = frameToAffine to
--     f v = mto #> v
--     g v = (mto #> v) ^+^ vto









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

-- data PlotType = HeatMap | Scatter | TimeSeries 




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
