module MatrixSpec where

import Matrix

import Test.Hspec

spec :: Spec
spec = do
    describe "Drawing" $ do
        describe "SquareMatrix2" $ do
            it "uses float equality comparison" $
                let m1 = SquareMatrix2 ((10.2^2, 5.2^2),
                                        (10.2^2, 5.2^2))
                    m2 = SquareMatrix2 ((104.04, 27.04),
                                        (104.04, 27.04))
                in m1 `shouldBe` m2
        describe "SquareMatrix3" $ do
            it "uses float equality comparison" $
                let m1 = SquareMatrix3 ((10.2^2, 5.2^2, 10.2^2),
                                        (10.2^2, 5.2^2, 10.2^2),
                                        (10.2^2, 5.2^2, 10.2^2))
                    m2 = SquareMatrix3 ((104.04, 27.04, 104.04),
                                        (104.04, 27.04, 104.04),
                                        (104.04, 27.04, 104.04))
                in m1 `shouldBe` m2
        describe "SquareMatrix4" $ do
            it "uses float equality comparison" $
                let m1 = SquareMatrix4 ((10.2^2, 5.2^2, 10.2^2, 5.2^2),
                                        (10.2^2, 5.2^2, 10.2^2, 5.2^2),
                                        (10.2^2, 5.2^2, 10.2^2, 5.2^2),
                                        (10.2^2, 5.2^2, 10.2^2, 5.2^2))
                    m2 = SquareMatrix4 ((104.04, 27.04, 104.04, 27.04),
                                        (104.04, 27.04, 104.04, 27.04),
                                        (104.04, 27.04, 104.04, 27.04),
                                        (104.04, 27.04, 104.04, 27.04))
                in m1 `shouldBe` m2