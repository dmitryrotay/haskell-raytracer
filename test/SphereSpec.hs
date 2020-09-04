module SphereSpec where

import Drawing (Color (..))
import Materials (defaultMaterial, Material (..))
import Space (Point (..), Vector (..), normalize)
import Sphere
    ( Sphere (..)
    , createSphere
    , setTransform
    , normalAt
    , setMaterial
    )
import Test.Hspec
import Transform (identity, translation, scaling, rotationZ, (|<>|))

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "sphere" $ do
            it "constructs sphere with identity transformation" $
                let (sphere, _) = createSphere 0
                in getTransform sphere `shouldBe` identity
            it "constructs sphere with default material" $
                let (sphere, _) = createSphere 0
                in getMaterial sphere `shouldBe` defaultMaterial
        describe "setTransform" $ do
            it "returns sphere object with passed transformation" $
                let (sphere, _) = createSphere 0
                    t = translation 2 3 4
                    sphere' = setTransform sphere t
                in getTransform sphere' `shouldBe` t

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
        
        describe "setMaterial" $ do
            it "returns sphere with applied material" $
                let (sphere, _) = createSphere 0
                    material = Material (Color 1 1 1) 1 1 1 200
                    sphereWithMaterial = setMaterial sphere material
                in getMaterial sphereWithMaterial `shouldBe` material