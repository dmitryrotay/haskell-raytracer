{-# LANGUAGE TypeApplications #-}

module ShapesSpec where

import Common (epsilon)
import Data.Fixed (mod')
import Drawing (Color (..))
import Lights (PointLight (..))
import Materials (Material (..), defaultMaterial)
import Patterns
    ( Pattern (..)
    , setPatternTransform
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    )
import Ray (Ray (..))
import Shapes
    ( Intersection (..)
    , Computations (..)
    , Shape (..)
    , createSphere
    , createPlane
    , hit
    , localIntersect
    , intersect
    , setTransform
    , localNormalAt
    , normalAt
    , prepareComputations
    , lighting
    , getPatternColorAt
    , getPatternColorForObjectAt
    )
import Space (Point (..), Vector (..), normalize)
import Test.Hspec
import Test.QuickCheck
import Transform (identity, translation, scaling, rotationZ, (|<>|), transformPoint)

spec :: Spec
spec = do
    describe "Shapes" $ do
        let black = Color 0 0 0
            white = Color 1 1 1

        describe "Sphere" $ do
            describe "createSphere" $ do
                it "constructs sphere with identity transformation" $
                    let (sphere, _) = createSphere 0
                    in getShapeTransform sphere `shouldBe` identity
                it "constructs sphere with default material" $
                    let (sphere, _) = createSphere 0
                    in getShapeMaterial sphere `shouldBe` defaultMaterial
            
            describe "setTransform" $ do
                it "returns sphere object with passed transformation" $
                    let (sphere, _) = createSphere 0
                        t = translation 2 3 4
                        sphere' = setTransform sphere t
                    in getShapeTransform sphere' `shouldBe` t

            describe "normalAt" $ do
                it "computes the normal on a sphere at a point on the x axis" $
                    let (sphere, _) = createSphere 0
                        n = normalAt sphere (Point 1 0 0)
                    in n `shouldBe` Vector 1 0 0
                it "computes the normal on a sphere at a point on the y axis" $
                    let (sphere, _) = createSphere 0
                        n = normalAt sphere (Point 0 1 0)
                    in n `shouldBe` Vector 0 1 0
                it "computes the normal on a sphere at a point on the z axis" $
                    let (sphere, _) = createSphere 0
                        n = normalAt sphere (Point 0 0 1)
                    in n `shouldBe` Vector 0 0 1
                it "computes the normal on a sphere at a nonaxial point" $
                    let (sphere, _) = createSphere 0
                        n = normalAt sphere (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
                    in n `shouldBe` Vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
                it "returns normal as a normalized vector" $
                    let (sphere, _) = createSphere 0
                        n = normalAt sphere (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
                    in n `shouldBe` normalize n
                it "computes the normal on a translated sphere" $
                    let (sphere, _) = createSphere 0
                        transformedSphere = setTransform sphere (translation 0 1 0)
                        n = normalAt transformedSphere (Point 0 1.70711 (-0.70711))
                    in n `shouldBe` Vector 0 0.70711 (-0.70711)
                it "computes the normal on a transformed sphere" $
                    let (sphere, _) = createSphere 0
                        transformedSphere = setTransform sphere (rotationZ (pi / 5) |<>| scaling 1 0.5 1)
                        n = normalAt transformedSphere (Point 0 (sqrt 2 / 2) (-sqrt 2 / 2))
                    in n `shouldBe` Vector 0 0.97014 (-0.24254)
        
            describe "intersect" $ do
                it "computes intersection of sphere by ray at two points" $
                    let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                        (sphere, _) = createSphere 0
                        t1 = Intersection sphere 4.0
                        t2 = Intersection sphere 6.0
                    in intersection `shouldBe` [t1, t2]
                it "computes intersection of sphere by ray at a tangent" $
                    let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                        (sphere, _) = createSphere 0
                        t1 = Intersection sphere 5.0
                        t2 = Intersection sphere 5.0
                    in intersection `shouldBe` [t1, t2]
                it "returns no intersections when ray misses the sphere" $
                    let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                    in intersection `shouldSatisfy` null
                it "computes intersection of sphere by ray originating inside the sphere" $
                    let intersection = raySphereIntersection 0 0 0 0 0 1
                        (sphere, _) = createSphere 0
                        t1 = Intersection sphere (-1.0)
                        t2 = Intersection sphere 1.0
                    in intersection `shouldBe` [t1, t2]
                it "computes intersection of sphere by ray originating after the sphere" $
                    let intersection = raySphereIntersection 0 0 5 0 0 1
                        (sphere, _) = createSphere 0
                        t1 = Intersection sphere (-6.0)
                        t2 = Intersection sphere (-4.0)
                    in intersection `shouldBe` [t1, t2]
                it "transforms ray before computing intersection" $
                    let r = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                        (sphere, _) = createSphere 0
                        sphere' = setTransform sphere (scaling 2 2 2)
                        t1 = Intersection sphere' 3.0
                        t2 = Intersection sphere' 7.0
                    in intersect sphere' r `shouldBe` [t1, t2]
        
        describe "Plane" $ do
            describe "localNormalAt" $ do
                it "returns the same vector for any point of the plane" $
                    let (plane, _) = createPlane 0
                        n1 = localNormalAt plane (Point 0 0 0)
                        n2 = localNormalAt plane (Point 10 0 (-10))
                        n3 = localNormalAt plane (Point (-5) 0 150)
                        expected = Vector 0 1 0
                    in do
                        n1 `shouldBe` expected
                        n2 `shouldBe` expected
                        n3 `shouldBe` expected
            
            describe "localIntersect" $ do
                it "returns empty list for intersection with a ray parallel to the plane" $
                    let (plane, _) = createPlane 0
                        ray = Ray (Point 0 10 0) (Vector 0 0 1)
                        xs = localIntersect plane ray
                    in xs `shouldSatisfy` null
                it "returns empty list for intersection with a ray coplanar with the plane" $
                    let (plane, _) = createPlane 0
                        ray = Ray (Point 0 0 0) (Vector 0 0 1)
                        xs = localIntersect plane ray
                    in xs `shouldSatisfy` null
                it "returns a point of intersection of a ray from above" $
                    let (plane, _) = createPlane 0
                        ray = Ray (Point 0 1 0) (Vector 0 (-1) 0)
                        xs = localIntersect plane ray
                    in xs `shouldBe` [Intersection plane 1]
                it "returns a point of intersection of a ray from above" $
                    let (plane, _) = createPlane 0
                        ray = Ray (Point 0 (-1) 0) (Vector 0 1 0)
                        xs = localIntersect plane ray
                    in xs `shouldBe` [Intersection plane 1]

        describe "hit" $ do
            it "returns hit when all intersection have positive t" $
                let (s, _) = createSphere 0
                    i1 = Intersection s 1
                    i2 = Intersection s 2
                in hit [i1, i2] `shouldBe` Just i1
            it "returns only intersection with positive t" $
                let (s, _) = createSphere 0
                    i1 = Intersection s (-1)
                    i2 = Intersection s 1
                in hit [i2, i1] `shouldBe` Just i2
            it "returns Nothing when all intersections are negative" $
                let (s, _) = createSphere 0
                    i1 = Intersection s (-2)
                    i2 = Intersection s (-1)
                in hit [i1, i2] `shouldBe` Nothing
            it "returns lowest nonnegative intersection" $
                let (s, _) = createSphere 0
                    i1 = Intersection s 5
                    i2 = Intersection s 7
                    i3 = Intersection s (-3)
                    i4 = Intersection s 2
                in hit [i1, i2, i3, i4] `shouldBe` Just i4
        
        describe "prepareComputations" $ do
            it "computes outside hit" $
                let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    i = Intersection sphere 4
                    comps = prepareComputations i ray
                in do
                    getCompShape comps `shouldBe` sphere
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
                    getCompShape comps `shouldBe` sphere
                    getCompDistance comps `shouldBe` getDistance i
                    getCompPoint comps `shouldBe` Point 0 0 1
                    getCompEyeVector comps `shouldBe` Vector 0 0 (-1)
                    getCompNormalVector comps `shouldBe` Vector 0 0 (-1)
                    getIsInside comps `shouldBe` True

            it "offsets the point of the hit" $
                let ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    sphere' = setTransform sphere (translation 0 0 1)
                    i = Intersection sphere' 5
                    comps = prepareComputations i ray
                in do
                    getPointZ (getCompOverPoint comps) < (-epsilon / 2) `shouldBe` True
                    getPointZ (getCompPoint comps) > getPointZ (getCompOverPoint comps) `shouldBe` True
        
        describe "Pattern Color" $ do
            let stripe = createStripePattern white black
            
            describe "getPatternColorAt" $ do
                describe "for stripe pattern" $ do
                    it "returns constant value if changing Y and Z coordinates" $ property $
                        \y z -> stripe `getPatternColorAt` Point 0 y z `shouldBe` white
                    it "returns alternating values if changing X coordinate" $ property $
                        \x -> stripe `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
                
                describe "for gradient pattern" $ do
                    let gradient = createGradientPattern black white
                    it "returns constant value if changing Z and Y coordinates" $ property $
                        \y z -> gradient `getPatternColorAt` Point 0 y z `shouldBe` black
                    it "returns color linearly interpolated between the gradient's colors by X coordinate" $ property $
                        \x -> let fraction = snd @Int . properFraction $ x
                              in gradient `getPatternColorAt` Point x 0 0 `shouldBe` Color fraction fraction fraction
                
                describe "for ring pattern" $ do
                    let ring = createRingPattern white black
                    it "returns constant value if changing Z coordinate" $ property $
                        \z -> ring `getPatternColorAt` Point 0 0 z `shouldBe` white
                    it "returns alternating values if changing X and Y coordinates" $ property $
                        \x y -> ring `getPatternColorAt` Point x y 0 `shouldBe` if sqrt (x ** 2 + y ** 2) `mod'` 2 < 1 then white else black
            
            describe "getPatternColorAtObject" $ do
                it "respects object transformation" $ property $
                    let check = do
                            s <- choose (epsilon, 100.0)
                            x <- arbitrary
                            let (object, _) = createSphere 0
                                object' = setTransform object (scaling s s s)
                                point = Point x 0 0
                                transformedPoint = transformPoint point (getShapeInverseTransform object')
                            return $ getPatternColorForObjectAt stripe object' point
                                    `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
                    in check
                it "respects pattern transformation" $ property $
                    let check = do
                            s <- choose (epsilon, 100.0)
                            x <- arbitrary
                            let (object, _) = createSphere 0
                                point = Point x 0 0
                                patt' = setPatternTransform stripe (scaling s s s)
                                transformedPoint = transformPoint point (getPatternInverseTransform patt')
                            return $ getPatternColorForObjectAt patt' object point
                                    `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
                    in check
                it "respects both object and pattern transformations" $ property $
                    let check = do
                            s <- choose (epsilon, 9999)
                            t <- choose (epsilon, 9999)
                            x <- arbitrary
                            let (object, _) = createSphere 0
                                point = Point x 0 0
                                object' = setTransform object (scaling s s s)
                                patt' = setPatternTransform stripe (translation t 0 0)
                                transformedPoint = transformPoint point (getShapeInverseTransform object' |<>| getPatternInverseTransform patt')
                            return $ getPatternColorForObjectAt patt' object' point
                                    `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
                    in check
        
        describe "lighting" $ do
            let (material, position, normalVector) = lightingCommonParameters
            it "computes color when eye between the light and surface" $
                let eyeVector = Vector 0 0 (-1)
                    light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                    result = lighting material light position eyeVector normalVector False
                in result `shouldBe` Color 1.9 1.9 1.9
            it "computes color when eye between the light and surface, eye offset 45°" $
                let eyeVector = Vector 0 (sqrt 2 / 2) (sqrt 2 / 2)
                    light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                    result = lighting material light position eyeVector normalVector False
                in result `shouldBe` Color 1.0 1.0 1.0
            it "computes color with eye opposite surface, light offset 45°" $
                let eyeVector = Vector 0 0 (-1)
                    light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                    result = lighting material light position eyeVector normalVector False
                in result `shouldBe` Color 0.7364 0.7364 0.7364
            it "computes color with eye in the path of the reflection vector" $
                let eyeVector = Vector 0 (-sqrt 2 / 2) (-sqrt 2 / 2)
                    light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                    result = lighting material light position eyeVector normalVector False
                in result `shouldBe` Color 1.6364 1.6364 1.6364
            it "computes color with the light behind the surface" $
                let eyeVector = Vector 0 0 (-1)
                    light = PointLight (Point 0 0 10) (Color 1 1 1)
                    result = lighting material light position eyeVector normalVector False
                in result `shouldBe` Color 0.1 0.1 0.1
            it "computes color for the surface in the shadow" $
                let eyeVector = Vector 0 0 (-1)
                    light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                    inShadow = True
                    result = lighting material light position eyeVector normalVector inShadow
                in result `shouldBe` Color 0.1 0.1 0.1
            it "computes color with a pattern applied" $
                let patt = createStripePattern (Color 1 1 1) (Color 0 0 0)
                    material' = material { getAmbient = 1, getDiffuse = 0, getSpecular = 0, getPattern = Just patt }
                    eyeVector = Vector 0 0 (-1)
                    light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                in property $
                    \x -> lighting material' light (Point x 0 0) eyeVector normalVector False
                          `shouldBe` if x `mod'` 2 < 1 then white else black

lightingCommonParameters :: (Material, Point, Vector)
lightingCommonParameters = (defaultMaterial, Point 0 0 0, Vector 0 0 (-1))

raySphereIntersection :: Double -> Double -> Double -> Double -> Double -> Double -> [Intersection]
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        ray = Ray origin direction
        (sphere, _) = createSphere 0
    in sphere `intersect` ray
