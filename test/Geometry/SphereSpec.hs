module Geometry.SphereSpec where

import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Geometry (Intersection (..))
import Geometry.Sphere (sphere, intersect, SphereRayIntersection (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "intersect" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s 4.0
                    t2 = Intersection s 6.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s 5.0
                    t2 = Intersection s 5.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s (-1.0)
                    t2 = Intersection s 1.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s (-6.0)
                    t2 = Intersection s (-4.0)
                in intersection `shouldBe` SphereRayIntersection t1 t2

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> SphereRayIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        r = Ray origin direction
        (s, _) = sphere 0
    in intersect s r
