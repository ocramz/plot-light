{-# language FlexibleInstances, ScopedTypeVariables #-}
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
    it "works" $ 
      True `shouldBe` True
    prop "prop_matMultGroup [Float]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Float) v ) -> prop_multMatGroup m v
    prop "prop_matMultGroup [Double]: m <\\> (m #> v) ~= v" $
      \(PropDiagMatVec (m :: DiagMat2r Double) v ) -> prop_multMatGroup m v



-- * Properties

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

