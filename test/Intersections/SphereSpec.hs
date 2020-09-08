module Intersections.SphereSpec where

import Common (epsilon)
import Intersections (Intersection (..))
import Intersections.Sphere 
    ( SphereRayIntersection (..)
    , Computations (..)
    , intersect
    , prepareComputations
    )
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Sphere (Sphere (..), createSphere, setTransform)
import Transform (scaling, translation)
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
            
        describe "prepareComputations" $ do
            it "computes outside hit" $
                let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    i = Intersection sphere 4
                    comps = prepareComputations i ray
                in do
                    getCompObject comps `shouldBe` sphere
                    getCompDistance comps `shouldBe` getDistance i
                    getCompPoint comps `shouldBe` Point 0 0 (-1)
                    getCompEyeVector comps `shouldBe` Vector 0 0 (-1)
                    getCompNormalVector comps `shouldBe` Vector 0 0 (-1)
                    getIsInside comps `shouldBe` False
                                        
            it "computes inside hit" $
                let ray = Ray (Point 0 0 0) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    i = Intersection sphere 1
                    comps = prepareComputations i ray
                in do
                    getCompObject comps `shouldBe` sphere
                    getCompDistance comps `shouldBe` getDistance i
                    getCompPoint comps `shouldBe` Point 0 0 1
                    getCompEyeVector comps `shouldBe` Vector 0 0 (-1)
                    getCompNormalVector comps `shouldBe` Vector 0 0 (-1)
                    getIsInside comps `shouldBe` True

            it "offsets the point of the hit" $
                let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    sphere' = sphere { getTransform = translation 0 0 1}
                    i = Intersection sphere' 5
                    comps = prepareComputations i ray
                in do
                    getPointZ (getCompOverPoint comps) < (-epsilon / 2) `shouldBe` True
                    getPointZ (getCompPoint comps) > getPointZ (getCompOverPoint comps) `shouldBe` True

raySphereIntersection :: Double -> Double -> Double -> Double -> Double -> Double -> SphereRayIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        ray = Ray origin direction
        (sphere, _) = createSphere 0
    in sphere `intersect` ray
