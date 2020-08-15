module DrawingSpec where

import Drawing

import Test.Hspec

spec :: Spec
spec = do
    describe "Drawing" $ do
        describe "Color" $ do
            it "adds two colors producing color with added components" $
                Color 0.9 0.6 0.75 `addColor` Color 0.7 0.1 0.25 == Color 1.6 0.7 1.0
            it "subtracts two colors producing color with subtracted components" $
                Color 0.9 0.6 0.75 `subtractColor` Color 0.7 0.1 0.25 == Color 0.2 0.5 0.5
            it "multiplies color by scalar producing color with components multiplied by scalar" $
                Color 0.2 0.3 0.4 `multiplyByScalar` 2 == Color 0.4 0.6 0.8
            it "multiplies two colors producing color with corresponding components multiplied" $
                Color 1 0.2 0.4 `multiplyByColor` Color 0.9 1 0.1 == Color 0.9 0.2 0.04
        describe "Canvas" $ do
            it "creates canvas with correct initial state" $
                let c = canvas 10 20
                in width c == 10 && height c == 20 && length (pixels c) == 200 && all (== Color 0 0 0) (pixels c)
            it "sets pixel on the canvas" $
                let c = canvas 10 20
                    red = Color 1 0 0
                    c' = setPixel c 2 3 red
                in pixelAt c' 2 3 == red
