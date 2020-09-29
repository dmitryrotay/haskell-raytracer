module DrawingSpec where

import Drawing
    ( Color (..)
    , addColor
    , subtractColor
    , multiplyByScalar
    , multiplyByColor
    , Canvas (..)
    , blankCanvas
    , setPixel
    , pixelAt
    )

import Test.Hspec

spec :: Spec
spec = do
    describe "Color" $ do
        it "adds two colors producing color with added components" $
            Color 0.9 0.6 0.75 `addColor` Color 0.7 0.1 0.25 `shouldBe` Color 1.6 0.7 1.0
        it "subtracts two colors producing color with subtracted components" $
            Color 0.9 0.6 0.75 `subtractColor` Color 0.7 0.1 0.25 `shouldBe` Color 0.2 0.5 0.5
        it "multiplies color by scalar producing color with components multiplied by scalar" $
            Color 0.2 0.3 0.4 `multiplyByScalar` 2 `shouldBe` Color 0.4 0.6 0.8
        it "multiplies two colors producing color with corresponding components multiplied" $
            Color 1 0.2 0.4 `multiplyByColor` Color 0.9 1 0.1 `shouldBe` Color 0.9 0.2 0.04
    
    describe "Canvas" $ do
        it "creates canvas with correct initial state" $
            let canvas = blankCanvas 10 20
            in do getWidth canvas `shouldBe` 10
                  getHeight canvas `shouldBe` 20
                  length (getPixels canvas) `shouldBe` 200
                  all (== Color 0 0 0) (getPixels canvas) `shouldBe` True
        it "sets pixel on the canvas" $
            let red = Color 1 0 0
                canvas = setPixel (blankCanvas 10 20) 2 3 red
            in pixelAt canvas 2 3 `shouldBe` red

        it "setting pixel outside canvas yields the same canvas" $
            let red = Color 1 0 0
                blank = blankCanvas 10 20
            in setPixel blank 20 20 red `shouldBe` blank
