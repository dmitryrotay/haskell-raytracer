module GeometrySpec where

import Test.Hspec
import Geometry

spec :: Spec
spec = do
    describe "Geometry" $ do
        describe "Point" $ do
            it "compares using float equality" $
                Point (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Point 104.04 27.04 4.84
        
        describe "Vector" $ do
            it "compares using float equality" $
                Vector (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Vector 104.04 27.04 4.84
