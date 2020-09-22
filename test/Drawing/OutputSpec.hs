module Drawing.OutputSpec where

import Drawing (Color (..), blankCanvas, setPixel)
import Drawing.Output (canvasToPpm)
import Test.Hspec

spec :: Spec
spec = do
    describe "canvasToPpm" $ do
        it "writes correct PPM header" $
            let header = take 3 . lines $ canvasToPpm (blankCanvas 5 3)
            in header `shouldBe` ["P3", "5 3", "255"]
        it "writes correct PPM pixels data" $
            let c1 = Color 1.5 1 0
                c2 = Color 0 0.5 0
                c3 = Color (-0.5) 0 1
                canvas = foldl (\c (x, y, color) -> setPixel c x y color) (blankCanvas 5 3) [(0, 0, c1), (2, 1, c2), (4, 2, c3)]
            in (drop 3 . lines . canvasToPpm $ canvas)
                `shouldBe`
                ["255 255 0 0 0 0 0 0 0 0 0 0 0 0 0",
                    "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
                    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
        it "breaks PPM lines longer than 70 characters" $
            let color = Color 1 0.8 0.6
                canvas = foldr (\(x, y) c -> setPixel c x y color) (blankCanvas 10 2) [(x, y) | x <- [0..9], y <- [0..1]]
            in (drop 3 . lines . canvasToPpm $ canvas) `shouldBe`
                ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                ,"153 255 204 153 255 204 153 255 204 153 255 204 153"
                ,"255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                ,"153 255 204 153 255 204 153 255 204 153 255 204 153"]
        it "ends PPM output with new line" $
            let canvas = blankCanvas 5 3
                ppm = canvasToPpm canvas
            in last ppm `shouldBe` '\n'