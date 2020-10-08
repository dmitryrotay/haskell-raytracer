module ObjectsSpec where

import Common (epsilon)
import Data.Fixed (mod')
import Drawing (Color (..))
import Lights (PointLight (..))
import Objects
    ( computeObjectPerceivedColor
    , getPatternColorForObjectAt
    )
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns
    ( createStripePattern
    , createTestPattern
    , getPatternInverseTransform
    , setPatternTransform
    )
import Objects.Shapes
    ( Shape (..)
    , createSphere
    , setTransform
    )
import Space (Point (..), Vector (..))
import Test.Hspec
import Test.QuickCheck
import Transform (translation, scaling, (|<>|), transformPoint)

spec :: Spec
spec = do
    let black = Color 0 0 0
        white = Color 1 1 1          
        stripe = createStripePattern white black
        
    describe "getPatternColorAtObject" $ do
        it "respects object transformation" $ property $
            let check = do
                    s <- choose (epsilon, 9999)
                    x <- arbitrary
                    let object = setTransform createSphere (scaling s s s)
                        point = Point x 0 0
                        transformedPoint = transformPoint point (getShapeInverseTransform object)
                    return $ getPatternColorForObjectAt stripe object point
                            `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
            in check
        
        it "respects pattern transformation" $ property $
            let check = do
                    s <- choose (epsilon, 9999)
                    x <- arbitrary
                    let point = Point x 0 0
                        patt' = setPatternTransform stripe (scaling s s s)
                        transformedPoint = transformPoint point (getPatternInverseTransform patt')
                    return $ getPatternColorForObjectAt patt' createSphere point
                            `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
            in check
        
        it "respects both object and pattern transformations" $ property $
            let check = do
                    s <- choose (epsilon, 9999)
                    t <- choose (epsilon, 9999)
                    x <- arbitrary
                    let point = Point x 0 0
                        object = setTransform createSphere (scaling s s s)
                        patt' = setPatternTransform stripe (translation t 0 0)
                        transformedPoint = transformPoint point (getShapeInverseTransform object |<>| getPatternInverseTransform patt')
                    return $ getPatternColorForObjectAt patt' object point
                            `shouldBe` if getPointX transformedPoint `mod'` 2 < 1 then white else black
            in check

        describe "for test pattern" $ do
            it "respects pattern transformation" $ do
                let object = createSphere
                    patt = setPatternTransform createTestPattern (scaling 2 2 2)
                getPatternColorForObjectAt patt object (Point 2 3 4) `shouldBe` Color 1 1.5 2
            
            it "respects object transformation" $ do
                let object = setTransform createSphere (scaling 2 2 2)
                    patt = createTestPattern
                getPatternColorForObjectAt patt object (Point 2 3 4) `shouldBe` Color 1 1.5 2

            it "correctly computes color for combined pattern and object transformations" $ do
                let object = setTransform createSphere (scaling 2 2 2)
                    patt = setPatternTransform createTestPattern (translation 0.5 1 1.5)
                getPatternColorForObjectAt patt object (Point 2.5 3 3.5) `shouldBe` Color 0.75 0.5 0.25
    
    describe "computeObjectPerceivedColor" $ do
        let (material, object, position, normalVector) = lightingCommonParameters
        it "computes color when eye between the light and surface" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                result = computeObjectPerceivedColor material object light position eyeVector normalVector False
            in result `shouldBe` Color 1.9 1.9 1.9
        it "computes color when eye between the light and surface, eye offset 45°" $
            let eyeVector = Vector 0 (sqrt 2 / 2) (sqrt 2 / 2)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                result = computeObjectPerceivedColor material object light position eyeVector normalVector False
            in result `shouldBe` Color 1.0 1.0 1.0
        it "computes color with eye opposite surface, light offset 45°" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                result = computeObjectPerceivedColor material object light position eyeVector normalVector False
            in result `shouldBe` Color 0.7364 0.7364 0.7364
        it "computes color with eye in the path of the reflection vector" $
            let eyeVector = Vector 0 (-sqrt 2 / 2) (-sqrt 2 / 2)
                light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                result = computeObjectPerceivedColor material object light position eyeVector normalVector False
            in result `shouldBe` Color 1.6364 1.6364 1.6364
        it "computes color with the light behind the surface" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 10) (Color 1 1 1)
                result = computeObjectPerceivedColor material object light position eyeVector normalVector False
            in result `shouldBe` Color 0.1 0.1 0.1
        it "computes color for the surface in the shadow" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                inShadow = True
                result = computeObjectPerceivedColor material object light position eyeVector normalVector inShadow
            in result `shouldBe` Color 0.1 0.1 0.1
        it "computes color with a pattern applied" $
            let patt = createStripePattern (Color 1 1 1) (Color 0 0 0)
                material' = material { getAmbient = 1, getDiffuse = 0, getSpecular = 0, getPattern = Just patt }
                eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
            in property $
                \x -> computeObjectPerceivedColor material' object light (Point x 0 0) eyeVector normalVector False
                        `shouldBe` if x `mod'` 2 < 1 then white else black

lightingCommonParameters :: (Material, Shape, Point, Vector)
lightingCommonParameters = (defaultMaterial, createSphere, Point 0 0 0, Vector 0 0 (-1))
