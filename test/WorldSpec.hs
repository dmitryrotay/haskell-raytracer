module WorldSpec where

import Drawing (Color (..))
import Lights (PointLight (..))
import Objects.Intersections (Intersection (..), prepareComputations)
import Objects.Shapes (Shape (..), createSphere, setMaterial, setTransform)
import Objects.Materials (Material (..))
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Test.Hspec
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
    )

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
    
    describe "shareHit" $ do
        it "shades an intersection from the outside" $
            let world = defaultWorld
                ray = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                shape = head (getShapes world)
                i = Intersection shape 4
                comps = prepareComputations i ray
            in shadeHit world shape comps `shouldBe` Color 0.38066 0.47583 0.2855
        
        it "shades an intersection from the inside" $
            let world = defaultWorld
                light = PointLight (Point 0 0.25 0) (Color 1 1 1)
                world' = setLight world light
                ray = Ray (Point 0 0 0) (Vector 0 0 1)
                shape = getShapes world !! 1
                i = Intersection shape 0.5
                comps = prepareComputations i ray
            in shadeHit world' shape comps `shouldBe` Color 0.90498 0.90498 0.90498

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
    
    describe "shadeHit" $ do
        it "computes color when given an intersection in shadow" $
            let (sphere1, id1) = createSphere 0
                (sphere2, _) = createSphere id1
                sphere2' = setTransform sphere2 (translation 0 0 10)
                world = World [sphere1, sphere2'] (Just (PointLight (Point 0 0 (-10)) (Color 1 1 1)))
                ray = Ray (Point 0 0 5) (Vector 0 0 1)
                i = Intersection sphere2' 4 
                comps = prepareComputations i ray
            in shadeHit world sphere2' comps `shouldBe` Color 0.1 0.1 0.1
