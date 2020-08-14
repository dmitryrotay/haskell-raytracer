module SpaceSpec where

import Space

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Geometry" $ do
        describe "Point" $ do
            it "compares using float equality" $
                Point (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Point 104.04 27.04 4.84
        
        describe "Vector" $ do
            it "compares using float equality" $
                Vector (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Vector 104.04 27.04 4.84

        describe "Operations" $ do
            it "adds Point and Vector producing Vector with added coordinates" $
                Point 3 (-2) 5 `addS` Vector (-2) 3 1 == Point 1 1 6
            it "adds Vector and Vector producing Vector with added coordinates" $
                Vector 3 (-2) 5 `addS` Vector (-2) 3 1 == Vector 1 1 6
            it "subtracts Point from Point producing Vector with subtracted coordinates" $
                Point 3 2 1 `subtractS` Point 5 6 7 == Vector (-2) (-4) (-6)
            it "subtracts Vector from Point producing Point with subtracted coordinates" $
                Point 3 2 1 `subtractS` Vector 5 6 7 == Point (-2) (-4) (-6)
            it "subtracts Vector from Vector producing Vector with subtracted coordinates" $
                Vector 3 2 1 `subtractS` Vector 5 6 7 == Vector (-2) (-4) (-6)
            it "negates Point returning Point with negated coordinates" $
                negateS (Point 1 (-2) 3) == Point (-1) 2 (-3)
            it "negates Vector returning Vector with negated coordinates" $
                negateS (Vector 1 (-2) 3) == Vector (-1) 2 (-3)
            it "multiplies Vector by value producing Vector with coordinates multiplied by value" $
                Vector 1 (-2) 3 `multiplyS` 3.5 == Vector 3.5 (-7) 10.5
            it "divides Vector by value producing Vector with coordinates divided by value" $
                Vector 1 (-2) 3 `divideS` 2 == Vector 0.5 (-1) 1.5
            it "calculates magnitude of Vector 1 0 0 as 1" $
                magnitude (Vector 1 0 0) == 1
            it "calculates magnitude of Vector 0 1 0 as 1" $
                magnitude (Vector 0 1 0) == 1
            it "calculates magnitude of Vector 0 0 1 as 1" $
                magnitude (Vector 0 0 1) == 1
            it "calculates magnitude of Vector 1 2 3 as sqrt 14" $
                magnitude (Vector 1 2 3) == sqrt 14
            it "calculates magnitude of Vector (-1) (-2) (-3) as sqrt 14" $
                magnitude (Vector (-1) (-2) (-3)) == sqrt 14
            it "normalizes Vector 4 0 0 as Vector 1 0 0" $
                normalize (Vector 4 0 0) == Vector 1 0 0
            it "normalizes Vector 1 2 3 as Vector 0.26726 0.53452 0.80178" $
                normalize (Vector 1 2 3) == Vector 0.26726 0.53452 0.80178
            it "normalizes Vector 1 2 3 to Vector with magnitude == 1" $
                let normalized = normalize (Vector 1 2 3)
                in magnitude normalized `floatEq` 1.0
            it "calculates dot product of Vector 1 2 3 and Vector 2 3 4 as 20" $
                dot (Vector 1 2 3) (Vector 2 3 4) == 20
            it "calculates cross product of Vector 1 2 3 and Vector 2 3 4 as Vector (-1) 2 (-1)" $
                cross (Vector 1 2 3) (Vector 2 3 4) == Vector (-1) 2 (-1)
            it "calculates cross product of Vector 2 3 4 and Vector 1 2 3 as Vector 1 (-2) 1" $
                cross (Vector 2 3 4) (Vector 1 2 3) == Vector 1 (-2) 1

prop_add_Point_Vector :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_add_Point_Vector x1 y1 z1 x2 y2 z2 = Point x1 y1 z1 `addS` Vector x2 y2 z2 == Point (x1 + x2) (y1 + y2) (z1 + z2)

prop_add_Vector_Vector :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_add_Vector_Vector x1 y1 z1 x2 y2 z2 = Vector x1 y1 z1 `addS` Vector x2 y2 z2 == Vector (x1 + x2) (y1 + y2) (z1 + z2)

prop_subtract_Point_Vector :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_subtract_Point_Vector x1 y1 z1 x2 y2 z2 = Point x1 y1 z1 `subtractS` Vector x2 y2 z2 == Vector (x1 - x2) (y1 - y2) (z1 - z2)

prop_subtract_Vector_Vector :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_subtract_Vector_Vector x1 y1 z1 x2 y2 z2 = Vector x1 y1 z1 `subtractS` Vector x2 y2 z2 == Vector (x1 - x2) (y1 - y2) (z1 - z2)

prop_subtract_Point_Point :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_subtract_Point_Point x1 y1 z1 x2 y2 z2 = Point x1 y1 z1 `subtractS` Point x2 y2 z2 == Vector (x1 - x2) (y1 - y2) (z1 - z2)