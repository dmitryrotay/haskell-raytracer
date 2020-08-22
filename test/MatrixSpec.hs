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
            it "calculates determinant correctly" $
                let m = SquareMatrix2 ((1, 5),
                                       (-3, 2))
                in determinant2 m == 17
        describe "SquareMatrix3" $ do
            it "uses float equality comparison" $
                let m1 = SquareMatrix3 ((10.2^2, 5.2^2, 10.2^2),
                                        (10.2^2, 5.2^2, 10.2^2),
                                        (10.2^2, 5.2^2, 10.2^2))
                    m2 = SquareMatrix3 ((104.04, 27.04, 104.04),
                                        (104.04, 27.04, 104.04),
                                        (104.04, 27.04, 104.04))
                in m1 `shouldBe` m2
            it "extracting submatrix returns 2x2 matrix with given row and column removed" $
                let m = SquareMatrix3 ((1, 5, 0),
                                       (-3, 2, 7),
                                       (0, 6, -3))
                    expected = SquareMatrix2 ((-3, 2),
                                              (0, 6))
                in subMatrix3 m 0 2 `shouldBe` expected
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
            it "multiplication by tuple produces valid product tuple" $
                let m = SquareMatrix4 ((1, 2, 3, 4),
                                       (2, 4, 4, 2),
                                       (8, 6, 4, 1),
                                       (0, 0, 0, 1))
                    t = (1, 2, 3, 1)
                    expected = (18, 24, 33, 1)
                in m `multiplyTuple4` t `shouldBe` expected
            it "multiplication by identity matrix must return self" $
                let m = SquareMatrix4 ((0, 1, 2, 4),
                                       (1, 2, 4, 8),
                                       (2, 4, 8, 16),
                                       (4, 8, 16, 32))
                in m `multiply4` identityMatrix4 `shouldBe` m
            it "transpose produces matrix with swapped rows and columns" $
                let m = SquareMatrix4 ((0, 9, 3, 0),
                                       (9, 8, 0, 8),
                                       (1, 8, 5, 3),
                                       (0, 0, 5, 8))
                    expected = SquareMatrix4 ((0, 9, 1, 0),
                                              (9, 8, 8, 0),
                                              (3, 0, 5, 5),
                                              (0, 8, 3, 8))
                in transpose4 m `shouldBe` expected
            it "extracting submatrix returns 3x3 matrix with given row and column removed" $
                let m = SquareMatrix4 ((-6, 1, 1, 6),
                                       (-8, 5, 8, 6),
                                       (-1, 0, 8, 2),
                                       (-7, 1, -1, 1))
                    expected = SquareMatrix3 ((-6, 1, 6),
                                              (-8, 8, 6),
                                              (-7, -1, 1))
                in subMatrix4 m 2 1 `shouldBe` expected
            it "transpose of identity matrix produces identity matrix" $
                transpose4 identityMatrix4 `shouldBe` identityMatrix4
            