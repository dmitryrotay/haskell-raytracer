module TransformSpec where

import Matrix (inverse)
import Space (Point (..), Vector (..), transformPoint, transformVector)
import Test.Hspec
import Transform
    ( (|<>|)
    , translation
    , scaling
    , rotationX
    , rotationY
    , rotationZ
    , shearing
    , combine
    )

spec :: Spec
spec = do
    describe "Transform" $ do
        describe "translation" $ do
            it "translates point by given offsets" $
                let t = translation 5 (-3) 2
                    p = Point (-3) 4 5
                    expected = Point 2 1 7
                in p `transformPoint` t `shouldBe` expected
            it "inverted translation translates point in opposite direction" $
                let t = inverse $ translation 5 (-3) 2
                    p = Point (-3) 4 5
                    expected = Point (-8) 7 3
                    result = p `transformPoint` t
                in result `shouldBe` expected
            it "does not affect vectors" $
                let t = translation 5 (-3) 2
                    v = Vector (-3) 4 5
                in v `transformVector` t `shouldBe` v
        
        describe "scaling" $ do
            it "scales point coordinates by given axis factors" $
                let t = scaling 2 3 4
                    p = Point (-4) 6 8
                    expected = Point (-8) 18 32
                in p `transformPoint` t `shouldBe` expected
            it "scales vector coordinates by given axis factors" $
                let t = scaling 2 3 4
                    v = Vector (-4) 6 8
                    expected = Vector (-8) 18 32
                in v `transformVector` t `shouldBe` expected
            it "scales vector coordinates down if inverted" $
                let t = inverse $ scaling 2 3 4
                    v = Vector (-4) 6 8
                    expected = Vector (-2) 2 2
                    result = v `transformVector` t
                in result `shouldBe` expected
            it "reflects point if given negative factors" $
                let t = scaling (-1) 1 1
                    p = Point 2 3 4
                    expected = Point (-2) 3 4
                in p `transformPoint` t `shouldBe` expected
        
        describe "rotationX" $ do
            it "rotates a point around x axis" $
                let halfQuarter = rotationX (pi / 4)
                    fullQuarter = rotationX (pi / 2)
                    p = Point 0 1 0
                    expectedHalfQuarter = Point 0 (sqrt 2 / 2) (sqrt 2 / 2)
                    expectedFullQuarter = Point 0 0 1
                in do p `transformPoint` halfQuarter `shouldBe` expectedHalfQuarter
                      p `transformPoint` fullQuarter `shouldBe` expectedFullQuarter
            it "rotates in opposite rotation around x axis if inverted" $
                let t = inverse $ rotationX (pi / 4)
                    p = Point 0 1 0
                    expected = Point 0 (sqrt 2 / 2) (-sqrt 2 / 2)
                    result = p `transformPoint` t
                in result `shouldBe` expected
        
        describe "rotationY" $ do
            it "rotates a point around y axis" $
                let halfQuarter = rotationY (pi / 4)
                    fullQuarter = rotationY (pi / 2)
                    p = Point 0 0 1
                    expectedHalfQuarter = Point (sqrt 2 / 2) 0 (sqrt 2 / 2)
                    expectedFullQuarter = Point 1 0 0
                in do p `transformPoint` halfQuarter `shouldBe` expectedHalfQuarter
                      p `transformPoint` fullQuarter `shouldBe` expectedFullQuarter
        
        describe "rotationZ" $ do
            it "rotates a point around z axis" $
                let halfQuarter = rotationZ (pi / 4)
                    fullQuarter = rotationZ (pi / 2)
                    p = Point 0 1 0
                    expectedHalfQuarter = Point (-sqrt 2 / 2) (sqrt 2 / 2) 0
                    expectedFullQuarter = Point (-1) 0 0
                in do p `transformPoint` halfQuarter `shouldBe` expectedHalfQuarter
                      p `transformPoint` fullQuarter `shouldBe` expectedFullQuarter
        
        describe "shearing" $ do
            it "moves x in proportion to y" $
                let t = shearing 1 0 0 0 0 0
                    p = Point 2 3 4
                    expected = Point 5 3 4
                in p `transformPoint` t `shouldBe` expected
            it "moves x in proportion to z" $
                let t = shearing 0 1 0 0 0 0
                    p = Point 2 3 4
                    expected = Point 6 3 4
                in p `transformPoint` t `shouldBe` expected
            it "moves y in proportion to x" $
                let t = shearing 0 0 1 0 0 0
                    p = Point 2 3 4
                    expected = Point 2 5 4
                in p `transformPoint` t `shouldBe` expected
            it "moves y in proportion to z" $
                let t = shearing 0 0 0 1 0 0
                    p = Point 2 3 4
                    expected = Point 2 7 4
                in p `transformPoint` t `shouldBe` expected
            it "moves z in proportion to x" $
                let t = shearing 0 0 0 0 1 0
                    p = Point 2 3 4
                    expected = Point 2 3 6
                in p `transformPoint` t `shouldBe` expected
            it "moves z in proportion to y" $
                let t = shearing 0 0 0 0 0 1
                    p = Point 2 3 4
                    expected = Point 2 3 7
                in p `transformPoint` t `shouldBe` expected
        describe "combine" $ do
            it "produces the same result as with applying transformations one by one" $
                let p = Point 1 0 1
                    a = rotationX (pi / 2)
                    b = scaling 5 5 5
                    c = translation 10 5 7
                    oneByOne = p `transformPoint` (c |<>| b |<>| a)
                    combined = p `transformPoint` combine [a, b, c]
                in combined `shouldBe` oneByOne
