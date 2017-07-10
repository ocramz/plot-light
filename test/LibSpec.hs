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
    it "works" $ do
      True `shouldBe` True
    -- prop "propDMat 1" $
    --   (DiagMat2r )
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



-- * Properties

prop_multMatGroup :: (MatrixGroup m v, Eq v) => m -> v -> Bool
prop_multMatGroup m v = m <\> (m #> v) == v


-- * Newtypes and their Arbitrary instances

newtype V2r a = V2r { v2r :: V2 a } deriving (Eq, Show)
instance Num a => Arbitrary (V2r a) where
  arbitrary = arbitrary


newtype Mat2r a = M2r { m2r :: Mat2 a} deriving (Eq, Show)
instance Arbitrary (Mat2r a) where
  arbitrary = arbitrary

newtype DiagMat2r a = Dm2r { dm2r :: DiagMat2 a } deriving (Eq, Show)
instance Arbitrary (DiagMat2r a) where
  arbitrary = arbitrary


-- helpers

sized2 :: (Int -> Int -> Gen a) -> Gen a
sized2 f = sized $ \i -> sized $ \j -> f i j


genV2 :: (Arbitrary a, Num a) => Gen (V2 a)
genV2 = V2 <$> arbitrary <*> arbitrary

genDiagMat2 :: (Arbitrary a, Num a) => Gen (DiagMat2 a)
genDiagMat2 = DMat2 <$> arbitrary <*> arbitrary

genMat2 :: (Arbitrary a, Num a) => Gen (Mat2 a)
genMat2 = Mat2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

