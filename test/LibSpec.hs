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
  describe "Lib" $ do
    it "works" $ 
      True `shouldBe` True
    prop "prop_matSemigroup" $
      \(PropMatVec (m :: Mat2r Float) v ) -> prop_matSemigroup m v
    prop "prop_multMatGroup" $
      \(PropDiagMatVec (m :: DiagMat2r Float) v ) -> prop_multMatGroup m v
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



-- * Properties

prop_matSemigroup ::
  (MultiplicativeSemigroup m, LinearMap m v, Eps v) => m -> v -> Bool
prop_matSemigroup m v = (m ## m) #> v ~= (m #> (m #> v))

prop_multMatGroup :: (MatrixGroup m v, Eps v) => m -> v -> Bool
prop_multMatGroup m v = m <\> (m #> v) ~= v


data PropMatVec a = PropMatVec (Mat2r a) (V2r a) deriving (Eq, Show)
instance Arbitrary (PropMatVec Float) where
  arbitrary = PropMatVec <$> arbitrary <*> arbitrary

data PropDiagMatVec a = PropDiagMatVec (DiagMat2r a) (V2r a) deriving (Eq, Show)
instance Arbitrary (PropDiagMatVec Float) where
  arbitrary = PropDiagMatVec <$> arbitrary <*> arbitrary


-- * Newtypes and their Arbitrary instances


-- | Float
type V2r a = V2 a
type Mat2r a = Mat2 a
type DiagMat2r a = DiagMat2 a

instance Arbitrary (V2r Float) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary (Mat2r Float) where
  arbitrary = Mat2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (DiagMat2r Float) where
  arbitrary = DMat2 <$> arbitrary <*> arbitrary


-- | Double
instance Arbitrary (V2r Double) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary (Mat2r Double) where
  arbitrary = Mat2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (DiagMat2r Double) where
  arbitrary = DMat2 <$> arbitrary <*> arbitrary





-- helpers

sized2 :: (Int -> Int -> Gen a) -> Gen a
sized2 f = sized $ \i -> sized $ \j -> f i j


genV2 :: (Arbitrary a, Num a) => Gen (V2 a)
genV2 = V2 <$> arbitrary <*> arbitrary

genDiagMat2 :: (Arbitrary a, Num a) => Gen (DiagMat2 a)
genDiagMat2 = DMat2 <$> arbitrary <*> arbitrary

genMat2 :: (Arbitrary a, Num a) => Gen (Mat2 a)
genMat2 = Mat2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

