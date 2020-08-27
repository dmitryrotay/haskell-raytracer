module SphereSpec where

import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Sphere (sphere, intersect, Intersection (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "intersect" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                in intersection `shouldBe` Hit 4.0 6.0
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                in intersection `shouldBe` Hit 5.0 5.0
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                in intersection `shouldBe` Hit (-1.0) 1.0
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                in intersection `shouldBe` Hit (-6.0) (-4.0)

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> Intersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        r = Ray origin direction
        (s, _) = sphere 0
    in intersect s r
