module WorldSpec where

import Drawing (Color (..))
import Geometry.Sphere (Sphere (..))
import Geometry (Intersection (..))
import Lights (PointLight (..))
import Materials (Material (..))
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Test.HUnit
import Test.Hspec
import Transform (scaling)
import World (World (..), createWorld, defaultWorld, intersectWorld)

spec :: Spec
spec = do
    describe "World" $ do
        describe "createWorld" $ do
            it "returns empty world" $
                let world = createWorld
                in do
                    getObjects world `shouldBe` []
                    getLight world `shouldBe` Nothing
        describe "defaultWorld" $ do
            it "return world with two spheres and one light" $
                case defaultWorld of
                    World [s1, s2] l ->
                        do
                            getColor (getMaterial s1) `shouldBe` Color 0.8 1.0 0.6
                            getTransform s2 `shouldBe` scaling  0.5 0.5 0.5
                            l `shouldBe` Just (PointLight (Point (-10) 10 (-10)) (Color 1 1 1))
                    _ -> assertFailure "World must contain two spheres and a light"
        describe "intersectWorld" $ do
            it "intersects a world with a ray" $
                let world = defaultWorld
                    ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    xs = intersectWorld world ray
                in do
                    length xs `shouldBe` 4
                    getDistance (head xs) `shouldBe` 4
                    getDistance (xs !! 1) `shouldBe` 4.5
                    getDistance (xs !! 2) `shouldBe` 5.5
                    getDistance (xs !! 3) `shouldBe` 6