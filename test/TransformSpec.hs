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