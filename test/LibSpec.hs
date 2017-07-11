{-# language FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Graphics.Rendering.Plot.Light

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Graphics.Rendering.Plot.Light" $ do
    -- it "works" $ 
    --   True `shouldBe` True
    prop "V2 : additive group [Float]" $ \(v :: V2r Float) ->
      prop_V2_additiveGroup v `shouldBe` True
    prop "V2 : additive group [Double]" $ \(v :: V2 Double) ->
      prop_V2_additiveGroup v `shouldBe` True
    prop "Mat2 : multiplicative group [Float]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Float) v ) -> prop_multMatGroup m v `shouldBe` True
    prop "Mat2 : multiplicative group [Double]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Double) v ) -> prop_multMatGroup m v `shouldBe` True
    it "Frame : frameToFrame works" $ do
      let from = Frame (Point 5 1) (Point 8 3) :: Frame Double
          to = Frame (Point 1 2) (Point 4 4)
          v1 = v2fromPoint $ Point 7 2
          mopt = diagMat2 1 1 :: DiagMat2 Double
          vopt = V2 0 0 
          v2 = frameToFrame from to mopt vopt v1
          p2 = movePoint v2 origin
      -- p2 `shouldBe` Point 3 3
      norm2 (p2 -. Point 3 3) ~= 0 `shouldBe` True
      










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

