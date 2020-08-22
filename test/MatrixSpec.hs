module MatrixSpec where

import qualified Matrix as M

import Test.Hspec

spec :: Spec
spec = do
    describe "Matrix" $ do
        describe "SquareMatrix" $ do
            it "uses float equality comparison" $
                let m1 = M.square2 (10.2^2, 5.2^2,
                                    10.2^2, 5.2^2)
                    m2 = M.square2 (104.04, 27.04,
                                    104.04, 27.04)
                in m1 `shouldBe` m2
        describe "determinant" $ do
            it "computes for 2x2 matrix correctly" $
                let m = M.square2 (1, 5,
                                   -3, 2)
                in M.determinant m `shouldBe` 17
            it "computes for 3x3 matrix correctly" $
                let m = M.square3 (1, 2, 6,
                                   -5, 8, -4,
                                   2, 6, 4)
                in M.determinant m `shouldBe` -196
            it "computes for 4x4 matrix correctly" $
                let m = M.square4 (-2, -8, 3, 5,
                                   -3, 1, 7, 3,
                                   1, 2, -9, 6,
                                   -6, 7, 7, -9)
                in M.determinant m `shouldBe` -4071
        describe "submatrix" $ do
            it "extracts submatrix from 3x3 matrix returning 2x2 matrix with given row and column removed" $
                let m = M.square3 (1, 5, 0,
                                   -3, 2, 7,
                                   0, 6, -3)
                    expected = M.square2 (-3, 2,
                                          0, 6)
                in M.submatrix m 0 2 `shouldBe` expected
        describe "minor" $ do
            it "computes for element of 3x3 matrix as determinant with row and column of the element" $
                let m = M.square3 (3, 5, 0,
                                   2, -1, -7,
                                   6, -1, 5)
                in M.minor m 1 0 `shouldBe` M.determinant (M.submatrix m 1 0)
        describe "cofactor" $ do
            it "computes for 0 0 for 3x3 matrix as value of 'minor'" $
                let m = M.square3 (3, 5, 0,
                                   2, -1, -7,
                                   6, -1, 5)
                in M.cofactor m 0 0 `shouldBe` M.minor m 0 0
            it "computes for 1 0 as minor as negated value of 'minor'" $
                let m = M.square3 (3, 5, 0,
                                   2, -1, -7,
                                   6, -1, 5)
                in M.cofactor m 1 0 `shouldBe` negate (M.minor m 1 0)
        describe "multiply" $ do
            it "multiplies two matrices returning valid product matrix" $
                let m1 = M.square4 (1, 2, 3, 4,
                                    5, 6, 7, 8,
                                    9, 8, 7, 6,
                                    5, 4, 3, 2)
                    m2 = M.square4 (-2, 1, 2, 3,
                                    3, 2, 1, -1,
                                    4, 3, 6, 5,
                                    1, 2, 7, 8)
                    expected = M.square4 (20, 22, 50, 48,
                                          44, 54, 114, 108,
                                          40, 58, 110, 102,
                                          16, 26, 46, 42)
                    result = m1 `M.multiply` m2
                    test = (case result of
                                (Left error) -> expectationFailure error
                                (Right result') -> result' `shouldBe` expected)
                in test
            it "multiplies by tuple returning valid product tuple" $
                let m = M.square4 (1, 2, 3, 4,
                                   2, 4, 4, 2,
                                   8, 6, 4, 1,
                                   0, 0, 0, 1)
                    t = M.vector4 (1, 2, 3, 1)
                    expected = M.vector4 (18, 24, 33, 1)
                    result = m `M.multiply` t
                    test = (case result of
                                (Left error) -> expectationFailure error
                                (Right result') -> result' `shouldBe` expected)
                in test
            it "multiplies by identity matrix returning original matrix" $
                let m = M.square4 (0, 1, 2, 4,
                                   1, 2, 4, 8,
                                   2, 4, 8, 16,
                                   4, 8, 16, 32)
                    result = m `M.multiply` M.identity 4
                    test = (case result of
                                (Left error) -> expectationFailure error
                                (Right result') -> result' `shouldBe` m)
                in test
            it "fails on mismatch of first matrix rows count and second matrix column count" $
                let m1 = M.square4 (0, 1, 2, 4,
                                   1, 2, 4, 8,
                                   2, 4, 8, 16,
                                   4, 8, 16, 32)
                    m2 = M.square3 (0, 1, 2, 
                                   1, 2, 4,
                                   2, 4, 8) 
                    result = m1 `M.multiply` m2
                    test = (case result of
                                (Left error) -> pure ()
                                (Right _) -> expectationFailure "Must fail if matrices rows/columns length don't match")
                in test
        describe "transpose" $ do
            it "produces matrix with swapped rows and columns" $
                let m = M.square4 (0, 9, 3, 0,
                                   9, 8, 0, 8,
                                   1, 8, 5, 3,
                                   0, 0, 5, 8)
                    expected = M.square4 (0, 9, 1, 0,
                                          9, 8, 8, 0,
                                          3, 0, 5, 5,
                                          0, 8, 3, 8)
                in M.transpose m `shouldBe` expected
            it "produces identity matrix from identity matrix" $
                M.transpose (M.identity 4) `shouldBe` M.identity 4
        describe "submatrix" $ do
            it "extracts submatrix from 4x4 matrix returning 3x3 matrix with given row and column removed" $
                let m = M.square4 (-6, 1, 1, 6,
                                   -8, 5, 8, 6,
                                   -1, 0, 8, 2,
                                   -7, 1, -1, 1)
                    expected = M.square3 (-6, 1, 6,
                                          -8, 8, 6,
                                          -7, -1, 1)
                in M.submatrix m 2 1 `shouldBe` expected
            