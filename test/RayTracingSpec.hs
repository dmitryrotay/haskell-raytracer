module RayTracingSpec where

import RayTracing (Ray(..), position)
import Space (Vector (..), Point (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "RayTracing" $ do
        describe "Ray" $ do
            it "constructs ray from point and vector" $
                let p = Point  1 2 3
                    v = Vector 4 5 6
                    r = Ray p v
                in do 
                    origin r `shouldBe` p
                    direction r `shouldBe` v
        describe "position" $ do
            it "calculates position from ray and distance" $
                let r = Ray (Point 2 3 4) (Vector 1 0 0)
                in do
                    position r 0 `shouldBe` Point 2 3 4
                    position r 1 `shouldBe` Point 3 3 4
                    position r (-1) `shouldBe` Point 1 3 4
                    position r 2.5 `shouldBe` Point 4.5 3 4