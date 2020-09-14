module MaterialsSpec where

import Drawing (Color (..))
import Lights (PointLight (..))
import Materials (Material (..), defaultMaterial, lighting)
import Patterns (createStripePattern)
import Space (Vector (..), Point (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "lighting" $ do
        let (material, position, normalVector) = commonParameters
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
            in do
                lighting material' light (Point 0.9 0 0) eyeVector normalVector False `shouldBe` Color 1 1 1
                lighting material' light (Point 1.1 0 0) eyeVector normalVector False `shouldBe` Color 0 0 0

commonParameters :: (Material, Point, Vector)
commonParameters = (defaultMaterial, Point 0 0 0, Vector 0 0 (-1))
