module TransformSpec where

import           Control.Monad
import           Matrix ((|*|))
import qualified Matrix as M
import qualified Space as S
import           Test.Hspec
import qualified Transform as T

spec :: Spec
spec = do
    describe "Transform" $ do
        describe "translation" $ do
            it "translates point by given offsets" $
                let t = T.translation 5 (-3) 2
                    p = M.fromPoint $ S.Point (-3) 4 5
                    expected = M.fromPoint $ S.Point 2 1 7
                in t |*| p `shouldBe` Right expected
            it "inverted translation translates point in opposite direction" $
                let t = M.inverse $ T.translation 5 (-3) 2
                    p = M.fromPoint $ S.Point (-3) 4 5
                    expected = M.fromPoint $ S.Point (-8) 7 3
                    result = t >>= (|*| p)
                in result `shouldBe` Right expected
            it "does not affect vectors" $
                let t = T.translation 5 (-3) 2
                    v = M.fromVector $ S.Vector (-3) 4 5
                in t |*| v `shouldBe` Right v
        describe "scaling" $ do
            it "scales point coordinates by given axis factors" $
                let t = T.scaling 2 3 4
                    p = M.fromPoint $ S.Point (-4) 6 8
                    expected = M.fromPoint $ S.Point (-8) 18 32
                in t |*| p `shouldBe` Right expected
            it "scales vector coordinates by given axis factors" $
                let t = T.scaling 2 3 4
                    v = M.fromVector $ S.Vector (-4) 6 8
                    expected = M.fromVector $ S.Vector (-8) 18 32
                in t |*| v `shouldBe` Right expected
            it "scales vector coordinates down if inverted" $
                let t = M.inverse $ T.scaling 2 3 4
                    v = M.fromVector $ S.Vector (-4) 6 8
                    expected = M.fromVector $ S.Vector (-2) 2 2
                    result = t >>= (|*| v)
                in result `shouldBe` Right expected
            it "reflects point if given negative factors" $
                let t = T.scaling (-1) 1 1
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point (-2) 3 4
                in t |*| p `shouldBe` Right expected
        describe "rotationX" $ do
            it "rotates a point around x axis" $
                let halfQuarter = T.rotationX (pi / 4)
                    fullQuarter = T.rotationX (pi / 2)
                    p = M.fromPoint $ S.Point 0 1 0
                    expectedHalfQuarter = M.fromPoint $ S.Point 0 (sqrt 2 / 2) (sqrt 2 / 2)
                    expectedFullQuarter = M.fromPoint $ S.Point 0 0 1
                in do halfQuarter |*| p `shouldBe` Right expectedHalfQuarter
                      fullQuarter |*| p `shouldBe` Right expectedFullQuarter
            it "rotates in opposite rotation around x axis if inverted" $
                let t = M.inverse $ T.rotationX (pi / 4)
                    p = M.fromPoint $ S.Point 0 1 0
                    expected = M.fromPoint $ S.Point 0 (sqrt 2 / 2) (-sqrt 2 / 2)
                    result = t >>= (|*| p)
                in result `shouldBe` Right expected
        describe "rotationY" $ do
            it "rotates a point around y axis" $
                let halfQuarter = T.rotationY (pi / 4)
                    fullQuarter = T.rotationY (pi / 2)
                    p = M.fromPoint $ S.Point 0 0 1
                    expectedHalfQuarter = M.fromPoint $ S.Point (sqrt 2 / 2) 0 (sqrt 2 / 2)
                    expectedFullQuarter = M.fromPoint $ S.Point 1 0 0
                in do halfQuarter |*| p `shouldBe` Right expectedHalfQuarter
                      fullQuarter |*| p `shouldBe` Right expectedFullQuarter
        describe "rotationZ" $ do
            it "rotates a point around z axis" $
                let halfQuarter = T.rotationZ (pi / 4)
                    fullQuarter = T.rotationZ (pi / 2)
                    p = M.fromPoint $ S.Point 0 1 0
                    expectedHalfQuarter = M.fromPoint $ S.Point (-sqrt 2 / 2) (sqrt 2 / 2) 0
                    expectedFullQuarter = M.fromPoint $ S.Point (-1) 0 0
                in do halfQuarter |*| p `shouldBe` Right expectedHalfQuarter
                      fullQuarter |*| p `shouldBe` Right expectedFullQuarter
        describe "shearing" $ do
            it "moves x in proportion to y" $
                let t = T.shearing 1 0 0 0 0 0
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 5 3 4
                in t |*| p `shouldBe` Right expected
            it "moves x in proportion to z" $
                let t = T.shearing 0 1 0 0 0 0
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 6 3 4
                in t |*| p `shouldBe` Right expected
            it "moves y in proportion to x" $
                let t = T.shearing 0 0 1 0 0 0
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 2 5 4
                in t |*| p `shouldBe` Right expected
            it "moves y in proportion to z" $
                let t = T.shearing 0 0 0 1 0 0
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 2 7 4
                in t |*| p `shouldBe` Right expected
            it "moves z in proportion to x" $
                let t = T.shearing 0 0 0 0 1 0
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 2 3 6
                in t |*| p `shouldBe` Right expected
            it "moves z in proportion to y" $
                let t = T.shearing 0 0 0 0 0 1
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point 2 3 7
                in t |*| p `shouldBe` Right expected
        describe "combine" $ do
            it "produces the same result as with applying transformations one by one" $
                let p = M.fromPoint $ S.Point 1 0 1
                    a = T.rotationX (pi / 2)
                    b = T.scaling 5 5 5
                    c = T.translation 10 5 7
                    oneByOne = do
                        ta <- a |*| p
                        tb <- b |*| ta
                        c |*| tb
                    combined = do
                        t <- T.combine [a, b, c]
                        t |*| p
                    in oneByOne `shouldBe` combined

