module TransformSpec where

import Control.Monad
import Matrix ((|*|), fromPoint, fromVector, inverse)
import Space (Point (..), Vector (..))
import Test.Hspec
import Transform ((|<>|), translation, scaling, rotationX, rotationY, rotationZ, shearing, combine)

spec :: Spec
spec = do
    describe "Transform" $ do
        describe "translation" $ do
            it "translates point by given offsets" $
                let t = translation 5 (-3) 2
                    p = fromPoint $ Point (-3) 4 5
                    expected = fromPoint $ Point 2 1 7
                in p |<>| t `shouldBe` Right expected
            it "inverted translation translates point in opposite direction" $
                let t = inverse $ translation 5 (-3) 2
                    p = fromPoint $ Point (-3) 4 5
                    expected = fromPoint $ Point (-8) 7 3
                    result = t >>= (p |<>|)
                in result `shouldBe` Right expected
            it "does not affect vectors" $
                let t = translation 5 (-3) 2
                    v = fromVector $ Vector (-3) 4 5
                in v |<>| t `shouldBe` Right v
        
        describe "scaling" $ do
            it "scales point coordinates by given axis factors" $
                let t = scaling 2 3 4
                    p = fromPoint $ Point (-4) 6 8
                    expected = fromPoint $ Point (-8) 18 32
                in p |<>| t `shouldBe` Right expected
            it "scales vector coordinates by given axis factors" $
                let t = scaling 2 3 4
                    v = fromVector $ Vector (-4) 6 8
                    expected = fromVector $ Vector (-8) 18 32
                in v |<>| t `shouldBe` Right expected
            it "scales vector coordinates down if inverted" $
                let t = inverse $ scaling 2 3 4
                    v = fromVector $ Vector (-4) 6 8
                    expected = fromVector $ Vector (-2) 2 2
                    result = t >>= (v |<>|)
                in result `shouldBe` Right expected
            it "reflects point if given negative factors" $
                let t = scaling (-1) 1 1
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point (-2) 3 4
                in p |<>| t `shouldBe` Right expected
        
        describe "rotationX" $ do
            it "rotates a point around x axis" $
                let halfQuarter = rotationX (pi / 4)
                    fullQuarter = rotationX (pi / 2)
                    p = fromPoint $ Point 0 1 0
                    expectedHalfQuarter = fromPoint $ Point 0 (sqrt 2 / 2) (sqrt 2 / 2)
                    expectedFullQuarter = fromPoint $ Point 0 0 1
                in do p |<>| halfQuarter `shouldBe` Right expectedHalfQuarter
                      p |<>| fullQuarter `shouldBe` Right expectedFullQuarter
            it "rotates in opposite rotation around x axis if inverted" $
                let t = inverse $ rotationX (pi / 4)
                    p = fromPoint $ Point 0 1 0
                    expected = fromPoint $ Point 0 (sqrt 2 / 2) (-sqrt 2 / 2)
                    result = t >>= (p |<>|)
                in result `shouldBe` Right expected
        
        describe "rotationY" $ do
            it "rotates a point around y axis" $
                let halfQuarter = rotationY (pi / 4)
                    fullQuarter = rotationY (pi / 2)
                    p = fromPoint $ Point 0 0 1
                    expectedHalfQuarter = fromPoint $ Point (sqrt 2 / 2) 0 (sqrt 2 / 2)
                    expectedFullQuarter = fromPoint $ Point 1 0 0
                in do p |<>| halfQuarter `shouldBe` Right expectedHalfQuarter
                      p |<>| fullQuarter `shouldBe` Right expectedFullQuarter
        
        describe "rotationZ" $ do
            it "rotates a point around z axis" $
                let halfQuarter = rotationZ (pi / 4)
                    fullQuarter = rotationZ (pi / 2)
                    p = fromPoint $ Point 0 1 0
                    expectedHalfQuarter = fromPoint $ Point (-sqrt 2 / 2) (sqrt 2 / 2) 0
                    expectedFullQuarter = fromPoint $ Point (-1) 0 0
                in do p |<>| halfQuarter `shouldBe` Right expectedHalfQuarter
                      p |<>| fullQuarter `shouldBe` Right expectedFullQuarter
        
        describe "shearing" $ do
            it "moves x in proportion to y" $
                let t = shearing 1 0 0 0 0 0
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 5 3 4
                in p |<>| t `shouldBe` Right expected
            it "moves x in proportion to z" $
                let t = shearing 0 1 0 0 0 0
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 6 3 4
                in p |<>| t `shouldBe` Right expected
            it "moves y in proportion to x" $
                let t = shearing 0 0 1 0 0 0
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 2 5 4
                in p |<>| t `shouldBe` Right expected
            it "moves y in proportion to z" $
                let t = shearing 0 0 0 1 0 0
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 2 7 4
                in p |<>| t `shouldBe` Right expected
            it "moves z in proportion to x" $
                let t = shearing 0 0 0 0 1 0
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 2 3 6
                in p |<>| t `shouldBe` Right expected
            it "moves z in proportion to y" $
                let t = shearing 0 0 0 0 0 1
                    p = fromPoint $ Point 2 3 4
                    expected = fromPoint $ Point 2 3 7
                in p |<>| t `shouldBe` Right expected
        describe "combine" $ do
            it "produces the same result as with applying transformations one by one" $
                let p = fromPoint $ Point 1 0 1
                    a = rotationX (pi / 2)
                    b = scaling 5 5 5
                    c = translation 10 5 7
                    oneByOne = do
                        ta <- p |<>| a
                        tb <- ta |<>| b
                        tb |<>| c
                    combined = do
                        t <- a |<>| b >>= (|<>| c)
                        p |<>| t
                    combinedWithFunc = do
                        t <- combine [a, b, c]
                        p |<>| t
                in do combined `shouldBe` oneByOne
                      combinedWithFunc `shouldBe` oneByOne
