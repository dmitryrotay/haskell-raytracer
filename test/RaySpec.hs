module RaySpec where

import Ray (Ray (..), position, transform)
import Space (Vector (..), Point (..))
import Transform (translation, scaling)
import Test.Hspec

spec :: Spec
spec = do
    describe "Ray" $ do
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
        describe "transform" $ do
            it "applies translation correctly" $
                let ray = Ray (Point 1 2 3) (Vector 0 1 0)
                    m = translation 3 4 5
                    r' = transform ray m
                in r' `shouldBe` Right (Ray (Point 4 6 8) (Vector 0 1 0))
            it "applies scaling correctly" $
                let ray = Ray (Point 1 2 3) (Vector 0 1 0)
                    m = scaling 2 3 4
                    r' = transform ray m
                in r' `shouldBe` Right (Ray (Point 2 6 12) (Vector 0 3 0))