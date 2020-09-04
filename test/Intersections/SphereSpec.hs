module Intersections.SphereSpec where

import Intersections (Intersection (..))
import Intersections.Sphere (SphereRayIntersection (..), intersect)
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Sphere (createSphere, setTransform)
import Transform (scaling)
import Test.Hspec

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "intersect" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere 4.0
                    t2 = Intersection sphere 6.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere 5.0
                    t2 = Intersection sphere 5.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere (-1.0)
                    t2 = Intersection sphere 1.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere (-6.0)
                    t2 = Intersection sphere (-4.0)
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "transforms ray before computing intersection" $
                let r = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    sphere' = setTransform sphere (scaling 2 2 2)
                    t1 = Intersection sphere' 3.0
                    t2 = Intersection sphere' 7.0
                in intersect sphere' r `shouldBe` SphereRayIntersection t1 t2

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> SphereRayIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        ray = Ray origin direction
        (sphere, _) = createSphere 0
    in sphere `intersect` ray
