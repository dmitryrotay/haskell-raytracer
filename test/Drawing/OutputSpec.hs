module Drawing.OutputSpec where

import Drawing
import Drawing.Output

import Test.Hspec

spec :: Spec
spec = do
    describe "Drawing.Output" $ do
        describe "PPM" $ do
            it "writes correct PPM header" $
                let header = take 3 . lines $ canvasToPpm (canvas 5 3)
                in header `shouldBe` ["P3", "5 3", "255"]
            it "writes correct PPM pixels data" $
                let c1 = Color 1.5 1 0
                    c2 = Color 0 0.5 0
                    c3 = Color (-0.5) 0 1
                    c = setPixel 0 0 c1 $ setPixel 2 1 c2 $ setPixel 4 2 c3 $ canvas 5 3
                in (drop 3 . lines . canvasToPpm $ c) `shouldBe` ["255 255 0 0 0 0 0 0 0 0 0 0 0 0 0", "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0", "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
            it "breaks PPM lines longer than 70 characters" $
                let color = Color 1 0.8 0.6
                    c = foldr (\(x, y) c -> setPixel x y color c) (canvas 10 2) [(x, y) | x <- [0..9], y <- [0..1]]
                in (drop 3 . lines . canvasToPpm $ c) `shouldBe`
                    ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                    ,"153 255 204 153 255 204 153 255 204 153 255 204 153"
                    ,"255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                    ,"153 255 204 153 255 204 153 255 204 153 255 204 153"]
            it "ends PPM output with new line" $
                let c = canvas 5 3
                    ppm = canvasToPpm c
                in last ppm `shouldBe` '\n'