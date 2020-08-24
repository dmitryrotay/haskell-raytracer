module TransformSpec where

import           Control.Monad
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
                in t `M.multiply` p `shouldBe` Right expected
            it "inverted translation translates point in opposite direction" $
                let t = M.inverse $ T.translation 5 (-3) 2
                    p = M.fromPoint $ S.Point (-3) 4 5
                    expected = M.fromPoint $ S.Point (-8) 7 3
                    result = t >>= (`M.multiply` p)
                in result `shouldBe` Right expected
            it "does not affect vectors" $
                let t = T.translation 5 (-3) 2
                    v = M.fromVector $ S.Vector (-3) 4 5
                in t `M.multiply` v `shouldBe` Right v
        describe "scaling" $ do
            it "scales point coordinates by given axis factors" $
                let t = T.scaling 2 3 4
                    p = M.fromPoint $ S.Point (-4) 6 8
                    expected = M.fromPoint $ S.Point (-8) 18 32
                in t `M.multiply` p `shouldBe` Right expected
            it "scales vector coordinates by given axis factors" $
                let t = T.scaling 2 3 4
                    v = M.fromVector $ S.Vector (-4) 6 8
                    expected = M.fromVector $ S.Vector (-8) 18 32
                in t `M.multiply` v `shouldBe` Right expected
            it "scales vector coordinates down if inverted" $
                let t = M.inverse $ T.scaling 2 3 4
                    v = M.fromVector $ S.Vector (-4) 6 8
                    expected = M.fromVector $ S.Vector (-2) 2 2
                    result = t >>= (`M.multiply` v)
                in result `shouldBe` Right expected
            it "reflects point if given negative factors" $
                let t = T.scaling (-1) 1 1
                    p = M.fromPoint $ S.Point 2 3 4
                    expected = M.fromPoint $ S.Point (-2) 3 4
                in t `M.multiply` p `shouldBe` Right expected
        describe "rotation" $ do
            it "rotates a point around x axis" $
                let halfQuarter = T.rotationX (pi / 4)
                    fullQuarter = T.rotationX (pi / 2)
                    p = M.fromPoint $ S.Point 0 1 0
                    expectedHalfQuarter = M.fromPoint $ S.Point 0 (sqrt 2 / 2) (sqrt 2 / 2)
                    expectedFullQuarter = M.fromPoint $ S.Point 0 0 1
                in do halfQuarter `M.multiply` p `shouldBe` Right expectedHalfQuarter
                      fullQuarter `M.multiply` p `shouldBe` Right expectedFullQuarter
            it "rotates in opposite rotation around x axis if inverted" $
                let t = M.inverse $ T.rotationX (pi / 4)
                    p = M.fromPoint $ S.Point 0 1 0
                    expected = M.fromPoint $ S.Point 0 (sqrt 2 / 2) (-sqrt 2 / 2)
                    result = t >>= (`M.multiply` p)
                in result `shouldBe` Right expected
