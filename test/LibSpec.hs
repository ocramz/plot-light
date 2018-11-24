{-# language FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.Internal.Geometry
-- import Data.TimeSeries

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TimeSeries" $ 
    it "correctly converts between `Tick`s and `Fractional`s" $ do
      let t0 = 57957.475
      fromTick (toTick t0) `shouldBe` t0
  describe "Graphics.Rendering.Plot.Light.Internal.Geometry" $ do
    prop "V2 : additive group [Float]" $ \(v :: V2r Float) ->
      prop_V2_additiveGroup v `shouldBe` True
    prop "V2 : additive group [Double]" $ \(v :: V2 Double) ->
      prop_V2_additiveGroup v `shouldBe` True
    prop "Mat2 : multiplicative group [Float]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Float) v ) -> prop_multMatGroup m v `shouldBe` True
    prop "Mat2 : multiplicative group [Double]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Double) v ) -> prop_multMatGroup m v `shouldBe` True
    it "Frame : frameToFrame works" $ do
      let from = Frame (mkV2 5 1) (mkV2 8 3) :: Frame (V2 Double)
          to = Frame (mkV2 1 2) (mkV2 4 4)
          p2 = frameToFrame from to (mkV2 7 2)
      norm2 (p2 ^-^ mkV2 3 3) ~= 0 `shouldBe` True
      



ps = [mkV2 12312 0.2, mkV2 13420.2 0.3, mkV2 14567 0.14]

fin = frameFromPoints ps
fout = mkFrame (mkV2 0 0) (mkV2 1 1)

-- vf = frameToFrame fin fout False False

v1 = V2 13555.2 0.25





-- * Properties

prop_V2_additiveGroup
  :: (Eps v, VectorSpace v, Num (Scalar v)) => v -> Bool
prop_V2_additiveGroup v =
  v ^+^ zero ~= v  &&
  (v ^+^ v) ~= (2 .* v) &&
  (v ^-^ v) ~= zero

prop_multMatGroup :: (MatrixGroup m v, Eps v) => m -> v -> Bool
prop_multMatGroup m v = m <\> (m #> v) ~= v




data PropMatVec a = PropMatVec (Mat2r a) (V2r a) deriving (Eq, Show)
instance Arbitrary (PropMatVec Float) where
  arbitrary = PropMatVec <$> arbitrary <*> arbitrary

data PropDiagMatVec a = PropDiagMatVec (DiagMat2r a) (V2r a) deriving (Eq, Show)
instance Arbitrary (PropDiagMatVec Float) where
  arbitrary = PropDiagMatVec <$> arbitrary <*> arbitrary

instance Arbitrary (PropDiagMatVec Double) where
  arbitrary = PropDiagMatVec <$> arbitrary <*> arbitrary





-- * Newtypes and their Arbitrary instances


-- | Float
type V2r a = V2 a
type Mat2r a = Mat2 a
type DiagMat2r a = DiagMat2 a

instance Arbitrary (V2r Float) where
  arbitrary = genV2

instance Arbitrary (Mat2r Float) where
  arbitrary = genMat2

instance Arbitrary (DiagMat2r Float) where
  arbitrary = genDiagMat2nz


-- | Double
instance Arbitrary (V2r Double) where
  arbitrary = genV2

instance Arbitrary (Mat2r Double) where
  arbitrary = genMat2

instance Arbitrary (DiagMat2r Double) where
  arbitrary = genDiagMat2nz





-- helpers

sized2 :: (Int -> Int -> Gen a) -> Gen a
sized2 f = sized $ \i -> sized $ \j -> f i j


genV2 :: (Arbitrary a, Num a) => Gen (V2 a)
genV2 = V2 <$> arbitrary <*> arbitrary

genDiagMat2 :: (Arbitrary a, Num a) => Gen (DiagMat2 a)
genDiagMat2 = DMat2 <$> arbitrary <*> arbitrary

genDiagMat2nz :: (Num a, Arbitrary a, Ord a) => Gen (DiagMat2 a)
genDiagMat2nz = do
  d1 <- arbitrary `suchThat` (> 0)
  d2 <- arbitrary `suchThat` (> 0)  
  pure (DMat2 d1 d2)

genMat2 :: (Arbitrary a, Num a) => Gen (Mat2 a)
genMat2 = Mat2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

