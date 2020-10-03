module Objects.MaterialsSpec where

import Drawing (Color (..))
import Objects.Materials (Material (..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Material" $ do
        it "constructs a material with correct parameters" $ property $
            \r g b a d s sh ref ->
                let material = Material (Color r g b) a d s sh ref Nothing
                in do
                    getColor material `shouldBe` Color r g b
                    getDiffuse material `shouldBe` d
                    getAmbient material `shouldBe` a
                    getSpecular material `shouldBe` s
                    getShininess material `shouldBe` sh
                    getReflective material `shouldBe` ref
                    getPattern material `shouldBe` Nothing
