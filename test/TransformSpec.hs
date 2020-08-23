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
                    test = (case t `M.multiply` p of
                             (Left error) -> expectationFailure error
                             (Right m) -> m `shouldBe` expected)
                in  test
            it "inverted translation translates point in opposite direction" $
                let t = M.inverse $ T.translation 5 (-3) 2
                    p = M.fromPoint $ S.Point (-3) 4 5
                    expected = M.fromPoint $ S.Point (-8) 7 3
                    result = t >>= (`M.multiply` p)
                    test = (case result of
                             (Left error) -> expectationFailure error
                             (Right m) -> m `shouldBe` expected)
                in  test
            it "does not affect vectors" $
                let t = T.translation 5 (-3) 2
                    v = M.fromVector $ S.Vector (-3) 4 5
                    test = (case t `M.multiply` v of
                             (Left error) -> expectationFailure error
                             (Right m) -> m `shouldBe` v)
                in  test