module WorldSpec where

import Drawing (Color (..))
import Lights (PointLight (..))
import Objects.Intersections (Intersection (..), prepareComputations)
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns (createTestPattern)
import Objects.Shapes (Shape (..), createSphere, createPlane, setMaterial, setTransform)
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Test.Hspec
import Test.QuickCheck
import Transform (scaling, translation)
import World
    ( World (..)
    , createWorld
    , defaultWorld
    , intersectWorld
    , shadeHit
    , setLight
    , colorAt
    , isShadowed
    , reflectedColor
    , reflectedColorWithBounceCount
    , refractedColor
    , refractedColorWithBounceCount
    )

{-# ANN module "HLint: ignore Reduce duplication" #-}
spec :: Spec
spec = do
    describe "createWorld" $ do
        it "returns empty world" $
            let world = createWorld
            in do
                getShapes world `shouldSatisfy` null
                getLight world `shouldBe` Nothing
    
    describe "defaultWorld" $ do
        it "return world with two spheres and one light" $
            case defaultWorld of
                World [s1, s2] l ->
                    do
                        getColor (getShapeMaterial s1) `shouldBe` Color 0.8 1.0 0.6
                        getShapeTransform s2 `shouldBe` scaling  0.5 0.5 0.5
                        l `shouldBe` Just (PointLight (Point (-10) 10 (-10)) (Color 1 1 1))
                _ -> expectationFailure "World must contain two spheres and a light"
    
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

    describe "colorAt" $ do
        it "returns black color when ray misses" $
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 1 0)
            in colorAt world ray `shouldBe` Color 0 0 0
        
        it "returns computed color when ray hits" $
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
            in colorAt world ray `shouldBe` Color 0.38066 0.47583 0.2855
        
        it "correctly computes color with an intersection behind the ray" $
            let world = defaultWorld
                outer = head (getShapes world)
                outer' = setMaterial outer (getShapeMaterial outer) { getAmbient = 1 }
                inner = getShapes world !! 1
                inner' = setMaterial inner (getShapeMaterial inner) { getAmbient = 1 }
                world' = world { getShapes = [inner', outer'] }
                ray = Ray (Point 0 0 0.75) (Vector 0 0 (-1))
            in colorAt world' ray `shouldBe` getColor (getShapeMaterial inner')
        
        it "successfully computes color for mutually reflective surfaces" $
            let light = PointLight (Point 0 0 0) (Color 1 1 1)
                lowerPlane = setTransform (createPlane { getShapeMaterial = defaultMaterial { getReflective = 1 } }) (translation 0 (-1) 0)
                upperPlane = setTransform (createPlane { getShapeMaterial = defaultMaterial { getReflective = 1 } }) (translation 0 1 0)
                world = World [lowerPlane, upperPlane] (Just light)
                ray = Ray (Point 0 0 0) (Vector 0 1 0)
                color = colorAt world ray
            in case color of
                 Color {} -> True

    describe "isShadowed" $ do
        it "returns False when nothing is collinear with point and light" $
            let world = defaultWorld
                point = Point 0 10 0
            in isShadowed world point `shouldBe` False
        it "returns True when an object is between the point and the light" $
            let world = defaultWorld
                point = Point 10 (-10) 10
            in isShadowed world point `shouldBe` True
        it  "returns False when an object is behind the light" $
            let world = defaultWorld
                point = Point (-20) 20 (-20)
            in isShadowed world point `shouldBe` False
        it  "returns False when an object is behind the point" $
            let world = defaultWorld
                point = Point (-2) 2 (-2)
            in isShadowed world point `shouldBe` False
        
        let worldWithoutLight = defaultWorld { getLight = Nothing }
        it "return True at any point for a World without light" $ property $
            \x y z -> isShadowed worldWithoutLight (Point x y z) `shouldBe` True
    
    describe "reflectedColor" $ do
        it "returns black color for non-reflective material" $ 
            let world = defaultWorld
                ray = Ray (Point 0 0 0) (Vector 0 0 1)
                shape = getShapes world !! 1
                shape' = shape { getShapeMaterial = (getShapeMaterial shape) { getAmbient = 1.0 } }
                i = Intersection shape' 1
                comps = prepareComputations i ray []
            in reflectedColor world comps `shouldBe` Color 0 0 0

        it "returns reflected color for reflective material" $
            let world = defaultWorld
                shape = setTransform (createPlane { getShapeMaterial = defaultMaterial { getReflective = 0.5 } }) (translation 0 (-1) 0)
                world' = world { getShapes = shape : getShapes world }
                ray = Ray (Point 0 0 (-3)) (Vector 0 (-sqrt 2 / 2) (sqrt 2 / 2))
                i = Intersection shape (sqrt 2)
                comps = prepareComputations i ray []
            in reflectedColor world' comps `shouldBe` Color 0.19032 0.2379 0.14274
        
        it "returns black color at maximum bounces count" $
            let world = defaultWorld
                shape = setTransform (createPlane { getShapeMaterial = defaultMaterial { getReflective = 0.5 } }) (translation 0 (-1) 0)
                world' = world { getShapes = shape : getShapes world }
                ray = Ray (Point 0 0 (-3)) (Vector 0 (-sqrt 2 / 2) (sqrt 2 / 2))
                i = Intersection shape (sqrt 2)
                comps = prepareComputations i ray []
            in reflectedColorWithBounceCount world' comps 0 `shouldBe` Color 0 0 0
    
    describe "refractedColor" $ do
        it "returns black color for opaque material" $ 
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                shape = head (getShapes world)
                intersections = [Intersection shape 4, Intersection shape 6]
                comps = prepareComputations (head intersections) ray intersections
            in refractedColor world comps `shouldBe` Color 0 0 0
        
        it "returns black color at maximum bounces count" $ 
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                shape = head (getShapes world)
                shape' = shape { getShapeMaterial = (getShapeMaterial shape) { getTransparency = 1.0, getRefractiveIndex = 1.5 } }
                intersections = [Intersection shape' 4, Intersection shape' 6]
                comps = prepareComputations (head intersections) ray intersections
            in refractedColorWithBounceCount world comps 0 `shouldBe` Color 0 0 0

        it "returns black color in case of total internal reflection" $ 
            let world = defaultWorld
                ray = Ray (Point 0 0 (sqrt 2 / 2)) (Vector 0 1 0)
                shape = head (getShapes world)
                shape' = shape { getShapeMaterial = (getShapeMaterial shape) { getTransparency = 1.0, getRefractiveIndex = 1.5 } }
                intersections = [Intersection shape' (-sqrt 2 / 2), Intersection shape' (sqrt 2 / 2)]
                comps = prepareComputations (intersections !! 1) ray intersections
            in refractedColor world comps `shouldBe` Color 0 0 0
        
        it "returns refracted color with a refracted ray" $
            let world = defaultWorld
                a = head (getShapes world)
                a' = a { getShapeMaterial = (getShapeMaterial a) { getAmbient = 1.0, getPattern = Just createTestPattern } }
                b = getShapes world !! 1
                b' = b { getShapeMaterial = (getShapeMaterial b) { getTransparency = 1.0, getRefractiveIndex = 1.5 } }
                world' = world { getShapes = [a', b'] }
                ray = Ray (Point 0 0 0.1) (Vector 0 1 0)
                intersections = [Intersection a' (-0.9899), Intersection b' (-0.4899), Intersection b' 0.4899, Intersection a' 0.9899]
                comps = prepareComputations (intersections !! 2) ray intersections
            in refractedColor world' comps `shouldBe` Color 0 0.99887 0.047218

    describe "shadeHit" $ do
        it "shades an intersection from the outside" $
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                shape = head (getShapes world)
                i = Intersection shape 4
                comps = prepareComputations i ray []
            in shadeHit world comps `shouldBe` Color 0.38066 0.47583 0.2855
        
        it "shades an intersection from the inside" $
            let world = defaultWorld
                light = PointLight (Point 0 0.25 0) (Color 1 1 1)
                world' = setLight world light
                ray = Ray (Point 0 0 0) (Vector 0 0 1)
                shape = getShapes world !! 1
                i = Intersection shape 0.5
                comps = prepareComputations i ray []
            in shadeHit world' comps `shouldBe` Color 0.90498 0.90498 0.90498
        
        it "returns black color if the world has no light" $
            let world = defaultWorld { getLight = Nothing}
                shape = getShapes world !! 1
                ray = Ray (Point 0 0 0) (Vector 0 0 1)
                i = Intersection shape 0.5 
                comps = prepareComputations i ray []
            in shadeHit world comps `shouldBe` Color 0 0 0
        
        it "computes color when given an intersection in shadow" $
            let sphere1 = createSphere
                sphere2' = setTransform createSphere (translation 0 0 10)
                world = World [sphere1, sphere2'] (Just (PointLight (Point 0 0 (-10)) (Color 1 1 1)))
                ray = Ray (Point 0 0 5) (Vector 0 0 1)
                i = Intersection sphere2' 4 
                comps = prepareComputations i ray []
            in shadeHit world comps `shouldBe` Color 0.1 0.1 0.1
        
        it "computes color for reflective material" $
            let world = defaultWorld
                shape = setTransform (createPlane { getShapeMaterial = defaultMaterial { getReflective = 0.5 } }) (translation 0 (-1) 0)
                world' = world { getShapes = shape : getShapes world }
                ray = Ray (Point 0 0 (-3)) (Vector 0 (-sqrt 2 / 2) (sqrt 2 / 2))
                i = Intersection shape (sqrt 2)
                comps = prepareComputations i ray []
            in shadeHit world' comps `shouldBe` Color 0.87677 0.92436 0.82918

        it "computes color for transparent material with refraction" $
            let world = defaultWorld
                floor' = setTransform (createPlane { getShapeMaterial = defaultMaterial { getTransparency = 0.5, getRefractiveIndex = 1.5 } }) (translation 0 (-1) 0)
                ball = setTransform (createSphere { getShapeMaterial = defaultMaterial { getColor = Color 1 0 0, getAmbient = 0.5 }}) (translation 0 (-3.5) (-0.5))
                world' = world { getShapes = ball : floor' : getShapes world }
                ray = Ray (Point 0 0 (-3)) (Vector 0 (-sqrt 2 /2) (sqrt 2 / 2))
                intersection = Intersection floor' (sqrt 2)
                comps = prepareComputations intersection ray [intersection]
            in shadeHit world' comps `shouldBe` Color 0.93642 0.68642 0.68642

        it "computes color for transparent and reflective material with refraction" $
            let world = defaultWorld
                floor' = setTransform
                            (createPlane { getShapeMaterial = defaultMaterial { getTransparency = 0.5, getReflective = 0.5, getRefractiveIndex = 1.5 } })
                            (translation 0 (-1) 0)
                ball = setTransform (createSphere { getShapeMaterial = defaultMaterial { getColor = Color 1 0 0, getAmbient = 0.5 }}) (translation 0 (-3.5) (-0.5))
                world' = world { getShapes = ball : floor' : getShapes world }
                ray = Ray (Point 0 0 (-3)) (Vector 0 (-sqrt 2 /2) (sqrt 2 / 2))
                intersection = Intersection floor' (sqrt 2)
                comps = prepareComputations intersection ray [intersection]
            in shadeHit world' comps `shouldBe` Color 0.93391 0.69643 0.69243
