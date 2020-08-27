module SphereSpec where

import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Sphere (sphere, intersect, RaySphereIntersection (..), SphereIntersection (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "intersect" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                    (s, _) = sphere 0
                in intersection `shouldBe` Intersection (SphereIntersection s 4.0) (SphereIntersection s 6.0)
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    (s, _) = sphere 0
                in intersection `shouldBe` Intersection (SphereIntersection s 5.0) (SphereIntersection s 5.0)
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    (s, _) = sphere 0
                in intersection `shouldBe` Intersection (SphereIntersection s (-1.0)) (SphereIntersection s 1.0)
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    (s, _) = sphere 0
                in intersection `shouldBe` Intersection (SphereIntersection s (-6.0)) (SphereIntersection s (-4.0))

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> RaySphereIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        r = Ray origin direction
        (s, _) = sphere 0
    in intersect s r
