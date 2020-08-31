module SpaceSpec where

import Common ((~==))
import Space
    ( Point (..)
    , Vector(..)
    , addVectorP
    , addVectorV
    , subtractPoint
    , subtractVectorP
    , subtractVectorV
    , multiplyVector
    , divideVector
    , magnitude
    , normalize
    , dot
    , cross
    , negateP
    , negateV
    )

import Test.Hspec

spec :: Spec
spec = do
    describe "Space" $ do
        describe "Point" $ do
            it "compares using float equality" $
                Point (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Point 104.04 27.04 4.84
        
        describe "Vector" $ do
            it "compares using float equality" $
                Vector (10.2 ^ 2) (5.2 ^ 2) (2.2 ^ 2) `shouldBe` Vector 104.04 27.04 4.84

        describe "Operations" $ do
            it "adds Point and Vector producing Vector with added coordinates" $
                Point 3 (-2) 5 `addVectorP` Vector (-2) 3 1 == Point 1 1 6
            it "adds Vector and Vector producing Vector with added coordinates" $
                Vector 3 (-2) 5 `addVectorV` Vector (-2) 3 1 == Vector 1 1 6
            it "subtracts Point from Point producing Vector with subtracted coordinates" $
                Point 3 2 1 `subtractPoint` Point 5 6 7 == Vector (-2) (-4) (-6)
            it "subtracts Vector from Point producing Point with subtracted coordinates" $
                Point 3 2 1 `subtractVectorP` Vector 5 6 7 == Point (-2) (-4) (-6)
            it "subtracts Vector from Vector producing Vector with subtracted coordinates" $
                Vector 3 2 1 `subtractVectorV` Vector 5 6 7 == Vector (-2) (-4) (-6)
            it "negates Point returning Point with negated coordinates" $
                negateP (Point 1 (-2) 3) == Point (-1) 2 (-3)
            it "negates Vector returning Vector with negated coordinates" $
                negateV (Vector 1 (-2) 3) == Vector (-1) 2 (-3)
            it "multiplies Vector by value producing Vector with coordinates multiplied by value" $
                Vector 1 (-2) 3 `multiplyVector` 3.5 == Vector 3.5 (-7) 10.5
            it "divides Vector by value producing Vector with coordinates divided by value" $
                Vector 1 (-2) 3 `divideVector` 2 == Vector 0.5 (-1) 1.5
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
                in magnitude normalized ~== 1.0
            it "calculates dot product of Vector 1 2 3 and Vector 2 3 4 as 20" $
                dot (Vector 1 2 3) (Vector 2 3 4) == 20
            it "calculates cross product of Vector 1 2 3 and Vector 2 3 4 as Vector (-1) 2 (-1)" $
                cross (Vector 1 2 3) (Vector 2 3 4) == Vector (-1) 2 (-1)
            it "calculates cross product of Vector 2 3 4 and Vector 1 2 3 as Vector 1 (-2) 1" $
                cross (Vector 2 3 4) (Vector 1 2 3) == Vector 1 (-2) 1
