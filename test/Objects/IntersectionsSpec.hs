module Objects.IntersectionsSpec where

import Common (epsilon, (~==))
import Objects.Intersections
    ( Computations (..)
    , Intersection (..)
    , hit
    , intersect
    , localIntersect
    , prepareComputations
    , schlick
    )
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Shapes (Shape (..), createGlassSphere, createPlane, createSphere, setTransform)
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Test.Hspec
import Transform (scaling, translation)

spec :: Spec
spec = do
    describe "hit" $ do
        it "returns hit when all intersection have positive t" $
            let s = createSphere
                i1 = Intersection s 1
                i2 = Intersection s 2
            in hit [i1, i2] `shouldBe` Just i1
        it "returns only intersection with positive t" $
            let s = createSphere
                i1 = Intersection s (-1)
                i2 = Intersection s 1
            in hit [i2, i1] `shouldBe` Just i2
        it "returns Nothing when all intersections are negative" $
            let s = createSphere
                i1 = Intersection s (-2)
                i2 = Intersection s (-1)
            in hit [i1, i2] `shouldBe` Nothing
        it "returns lowest nonnegative intersection" $
            let s = createSphere
                i1 = Intersection s 5
                i2 = Intersection s 7
                i3 = Intersection s (-3)
                i4 = Intersection s 2
            in hit [i1, i2, i3, i4] `shouldBe` Just i4

    describe "intersect" $ do
        describe "for Sphere" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                    sphere = createSphere
                    t1 = Intersection sphere 4.0
                    t2 = Intersection sphere 6.0
                in intersection `shouldBe` [t1, t2]
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    sphere = createSphere
                    t1 = Intersection sphere 5.0
                    t2 = Intersection sphere 5.0
                in intersection `shouldBe` [t1, t2]
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldSatisfy` null
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    sphere = createSphere
                    t1 = Intersection sphere (-1.0)
                    t2 = Intersection sphere 1.0
                in intersection `shouldBe` [t1, t2]
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    sphere = createSphere
                    t1 = Intersection sphere (-6.0)
                    t2 = Intersection sphere (-4.0)
                in intersection `shouldBe` [t1, t2]
            it "transforms ray before computing intersection" $
                let r = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    sphere = createSphere
                    sphere' = setTransform sphere (scaling 2 2 2)
                    t1 = Intersection sphere' 3.0
                    t2 = Intersection sphere' 7.0
                in intersect sphere' r `shouldBe` [t1, t2]
    
    describe "localIntersect" $ do
        describe "for Plane" $ do
            it "returns empty list for intersection with a ray parallel to the plane" $
                let plane = createPlane
                    ray = Ray (Point 0 10 0) (Vector 0 0 1)
                    xs = localIntersect plane ray
                in xs `shouldSatisfy` null
            it "returns empty list for intersection with a ray coplanar with the plane" $
                let plane = createPlane
                    ray = Ray (Point 0 0 0) (Vector 0 0 1)
                    xs = localIntersect plane ray
                in xs `shouldSatisfy` null
            it "returns a point of intersection of a ray from above" $
                let plane = createPlane
                    ray = Ray (Point 0 1 0) (Vector 0 (-1) 0)
                    xs = localIntersect plane ray
                in xs `shouldBe` [Intersection plane 1]
            it "returns a point of intersection of a ray from above" $
                let plane = createPlane
                    ray = Ray (Point 0 (-1) 0) (Vector 0 1 0)
                    xs = localIntersect plane ray
                in xs `shouldBe` [Intersection plane 1]
    
    describe "prepareComputations" $ do
        it "computes outside hit" $
            let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                sphere = createSphere
                i = Intersection sphere 4
                comps = prepareComputations i ray []
            in do
                getCompShape comps `shouldBe` sphere
                getCompDistance comps `shouldBe` getDistance i
                getCompPoint comps `shouldBe` Point 0 0 (-1)
                getCompEyeVector comps `shouldBe` Vector 0 0 (-1)
                getCompNormalVector comps `shouldBe` Vector 0 0 (-1)
                getIsInside comps `shouldBe` False
                                    
        it "computes inside hit" $
            let ray = Ray (Point 0 0 0) (Vector 0 0 1)
                sphere = createSphere
                i = Intersection sphere 1
                comps = prepareComputations i ray []
            in do
                getCompShape comps `shouldBe` sphere
                getCompDistance comps `shouldBe` getDistance i
                getCompPoint comps `shouldBe` Point 0 0 1
                getCompEyeVector comps `shouldBe` Vector 0 0 (-1)
                getCompNormalVector comps `shouldBe` Vector 0 0 (-1)
                getIsInside comps `shouldBe` True

        it "offsets the point of the hit" $ do
            let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                sphere' = setTransform createSphere (translation 0 0 1)
                i = Intersection sphere' 5
                comps = prepareComputations i ray []
            getPointZ (getCompOverPoint comps) < (-epsilon / 2) `shouldBe` True
            getPointZ (getCompPoint comps) > getPointZ (getCompOverPoint comps) `shouldBe` True
        
        it "computes the reflection vector" $ do
            let shape = createPlane
                ray = Ray (Point 0 1 (-1)) (Vector 0 (-sqrt 2 / 2) (sqrt 2 / 2))
                i = Intersection shape (sqrt 2)
                comps = prepareComputations i ray []
            getCompReflectionVector comps `shouldBe` Vector 0 (sqrt 2 / 2) (sqrt 2 / 2)
        
        it "finds n1 and n2 for refraction at various intersections" $ do
            let a = setTransform (createGlassSphere { getShapeMaterial = defaultMaterial { getRefractiveIndex = 1.5 } }) (scaling 2 2 2)
                b = setTransform (createGlassSphere { getShapeMaterial = defaultMaterial { getRefractiveIndex = 2.0 } }) (translation 0 0 (-0.25))
                c = setTransform (createGlassSphere { getShapeMaterial = defaultMaterial { getRefractiveIndex = 2.5 } }) (translation 0 0 0.25)
                ray = Ray (Point 0 0 (-4)) (Vector 0 0 1)
                intersections = 
                    [ Intersection a 2.0
                    , Intersection b 2.75
                    , Intersection c 3.25
                    , Intersection b 4.75
                    , Intersection c 5.25
                    , Intersection a 6
                    ]
                expectedResults =
                    [ (1.0, 1.5)
                    , (1.5, 2.0)
                    , (2.0, 2.5)
                    , (2.5, 2.5)
                    , (2.5, 1.5)
                    , (1.5 , 1.0)
                    ]
            map (\i -> let comps = prepareComputations i ray intersections
                        in (getCompN1 comps, getCompN2 comps)) intersections
                `shouldBe` expectedResults

        it "computes underPoint lying beneath the intersected surface" $ do
            let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                sphere = setTransform createGlassSphere (translation 0 0 1)
                i = Intersection sphere 5
                intersections = [i]
                comps = prepareComputations i ray intersections
                underPointZ = (getPointZ . getCompUnderPoint) comps
            underPointZ `shouldSatisfy` (> epsilon / 2)
            (getPointZ . getCompPoint) comps `shouldSatisfy` (< underPointZ)

    describe "schlick" $ do
        it "calculates result under total internal reflection" $ do
            let shape = createGlassSphere
                ray = Ray (Point 0 0 (sqrt 2 / 2)) (Vector 0 1 0)
                intersections = [Intersection shape (-sqrt 2 / 2), Intersection shape (sqrt 2 / 2)]
                comps = prepareComputations (intersections !! 1) ray intersections
            schlick comps `shouldBe` 1.0

        it "returns small reflectance with perpendicular viewing angle" $ do
            let shape = createGlassSphere
                ray = Ray (Point 0 0 0) (Vector 0 1 0)
                intersections = [Intersection shape (-1), Intersection shape 1]
                comps = prepareComputations (intersections !! 1) ray intersections
            schlick comps ~== 0.04 `shouldBe` True

        it "returns significant reflectance with small viewing angle and n2 > n1" $ do
            let shape = createGlassSphere
                ray = Ray (Point 0 0.99 (-2)) (Vector 0 0 1)
                intersection = Intersection shape 1.8589
                comps = prepareComputations intersection ray [intersection]
            schlick comps ~== 0.48873 `shouldBe` True
        
raySphereIntersection :: Double -> Double -> Double -> Double -> Double -> Double -> [Intersection]
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        ray = Ray origin direction
        sphere = createSphere
    in sphere `intersect` ray
