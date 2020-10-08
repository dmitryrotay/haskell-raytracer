module Objects.MaterialsSpec where

import Drawing (Color (..))
import Objects.Materials (Material (..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Material" $ do
        it "constructs a material with correct parameters" $ property $
            \r g b a d s sh refl refr t ->
                let material = Material a (Color r g b) d Nothing refl refr sh s t
                in do
                    getColor material `shouldBe` Color r g b
                    getDiffuse material `shouldBe` d
                    getAmbient material `shouldBe` a
                    getSpecular material `shouldBe` s
                    getShininess material `shouldBe` sh
                    getPattern material `shouldBe` Nothing
                    getReflective material `shouldBe` refl
                    getRefractiveIndex material `shouldBe` refr
                    getTransparency material `shouldBe` t
