module Objects.ShapesSpec where

import Objects.Materials (defaultMaterial)
import Objects.Shapes
    ( Shape (..)
    , ShapeType (..)
    , createSphere
    , createPlane
    , localNormalAt
    , normalAt
    , setTransform
    )
import Test.Hspec
import Test.QuickCheck
import Transform (identity, rotationZ, scaling, translation, (|<>|))
import Space (Vector (..), Point(..), normalize)

spec :: Spec
spec = do
    describe "createSphere" $ do
        let (sphere, _) = createSphere 0
        
        it "constructs sphere with identity transformation" $
            getShapeTransform sphere `shouldBe` identity
        it "constructs sphere with default material" $
            getShapeMaterial sphere `shouldBe` defaultMaterial
        it "constructs shape with correct ShapeType" $
            getShapeType sphere `shouldBe` Sphere
        it "constructs sphere with passed id value" $ property $
            \x -> let (newSphere, _) = createSphere x
                  in getShapeId newSphere `shouldBe` x
        it "returns next id along with the sphere object from constructor" $ property $
            \x -> let (_, nextId) = createSphere x
                  in nextId `shouldBe` x + 1
    
    describe "localNormalAt" $ do
        describe "for Plane" $ do
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

    describe "normalAt" $ do
        describe "for Sphere" $ do
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
    
    describe "setTransform" $ do
        describe "for Sphere" $ do
            it "returns sphere object with passed transformation" $
                let (sphere, _) = createSphere 0
                    t = translation 2 3 4
                    sphere' = setTransform sphere t
                in getShapeTransform sphere' `shouldBe` t
