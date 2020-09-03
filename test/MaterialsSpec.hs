module MaterialsSpec where

import Drawing (Color (..))
import Lights (PointLight (..))
import Materials (Material, defaultMaterial, lighting)
import Space (Vector (..), Point (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "lighting" $ do
        let (material, position, normalVector) = commonParameters

        it "computes color when eye between the light and surface" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                result = lighting material light position eyeVector normalVector
            in result `shouldBe` Color 1.9 1.9 1.9
        it "computes color when eye between the light and surface, eye offset 45°" $
            let eyeVector = Vector 0 (sqrt 2 / 2) (sqrt 2 / 2)
                light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
                result = lighting material light position eyeVector normalVector
            in result `shouldBe` Color 1.0 1.0 1.0
        it "computes color with eye opposite surface, light offset 45°" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                result = lighting material light position eyeVector normalVector
            in result `shouldBe` Color 0.7364 0.7364 0.7364
        it "computes color with eye in the path of the reflection vector" $
            let eyeVector = Vector 0 (-sqrt 2 / 2) (-sqrt 2 / 2)
                light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
                result = lighting material light position eyeVector normalVector
            in result `shouldBe` Color 1.63638 1.63638 1.63638
        it "computes color with the light behind the surface" $
            let eyeVector = Vector 0 0 (-1)
                light = PointLight (Point 0 0 10) (Color 1 1 1)
                result = lighting material light position eyeVector normalVector
            in result `shouldBe` Color 0.1 0.1 0.1

commonParameters :: (Material, Point, Vector)
commonParameters = (defaultMaterial, Point 0 0 0, Vector 0 0 (-1))