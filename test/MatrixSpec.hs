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
            it "multiplication of two matrices produces valid product matrix" $
                let m1 = SquareMatrix4 ((1, 2, 3, 4),
                                        (5, 6, 7, 8),
                                        (9, 8, 7, 6),
                                        (5, 4, 3, 2))
                    m2 = SquareMatrix4 ((-2, 1, 2, 3),
                                        (3, 2, 1, -1),
                                        (4, 3, 6, 5),
                                        (1, 2, 7, 8))
                    expected = SquareMatrix4 ((20, 22, 50, 48),
                                              (44, 54, 114, 108),
                                              (40, 58, 110, 102),
                                              (16, 26, 46, 42))
                in m1 `multiply4` m2 `shouldBe` expected
            it "multiplication of matrix by tuple produces valid product tuple" $
                let m = SquareMatrix4 ((1, 2, 3, 4),
                                       (2, 4, 4, 2),
                                       (8, 6, 4, 1),
                                       (0, 0, 0, 1))
                    t = (1, 2, 3, 1)
                    expected = (18, 24, 33, 1)
                in m `multiplyTuple4` t `shouldBe` expected