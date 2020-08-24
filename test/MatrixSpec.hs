module MatrixSpec where

import           Control.Monad
import qualified Matrix as M
import           Test.Hspec

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
                in m1 `M.multiply` m2 `shouldBe` Right expected
            it "multiplies by tuple returning valid product tuple" $
                let m = M.square4 (1, 2, 3, 4,
                                   2, 4, 4, 2,
                                   8, 6, 4, 1,
                                   0, 0, 0, 1)
                    t = M.fromTuple4 (1, 2, 3, 1)
                    expected = M.fromTuple4 (18, 24, 33, 1)
                in m `M.multiply` t `shouldBe` Right expected
            it "multiplies by identity matrix returning original matrix" $
                let m = M.square4 (0, 1, 2, 4,
                                   1, 2, 4, 8,
                                   2, 4, 8, 16,
                                   4, 8, 16, 32)
                    result = m `M.multiply` M.identity 4
                in m `M.multiply` M.identity 4 `shouldBe` Right m
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
        describe "inverse" $ do
            it "determines if matrix is invertible" $
                let m = M.square4 (6, 4, 4, 4,
                                   5, 5, 7, 6,
                                   4,-9, 3,-7,
                                   9, 1, 7,-6)
                in m `shouldSatisfy` M.invertible
            it "determines if matrix is not invertible" $
                let m = M.square4 (-4, 2,-2,-3,
                                    9, 6, 2, 6,
                                    0,-5, 1,-5,
                                    0, 0, 0, 0)
                in m `shouldNotSatisfy` M.invertible
            it "computes inverse matrix correctly 1" $
                let m = M.square4 (-5, 2, 6,-8,
                                    1,-5, 1, 8,
                                    7, 7,-6,-7,
                                    1,-3, 7, 4)
                    expected = M.square4 ( 0.21805,  0.45113,  0.24060, -0.04511,
                                          -0.80827, -1.45677, -0.44361,  0.52068,
                                          -0.07895, -0.22368, -0.05263,  0.19737,
                                          -0.52256, -0.81391, -0.30075,  0.30639)
                in M.inverse m `shouldBe` Right expected
            it "computes inverse matrix correctly 2" $
                let m = M.square4 ( 8,-5, 9, 2,
                                    7, 5, 6, 1,
                                   -6, 0, 9, 6,
                                   -3, 0,-9,-4)
                    expected = M.square4 (-0.15385, -0.15385, -0.28205, -0.53846,
                                          -0.07692,  0.12308,  0.02564,  0.03077,
                                           0.35897,  0.35897,  0.43590,  0.92308,
                                          -0.69231, -0.69231,  -0.76923, -1.92308)
                in M.inverse m `shouldBe` Right expected
            it "computes inverse matrix correctly 3" $
                let m = M.square4 ( 9, 3, 0, 9,
                                   -5,-2,-6,-3,
                                   -4, 9, 6, 4,
                                   -7, 6, 6, 2)
                    expected = M.square4 (-0.04074, -0.07778,  0.14444, -0.22222,
                                          -0.07778,  0.03333,  0.36667, -0.33333,
                                          -0.02901, -0.14630, -0.10926,  0.12963,
                                           0.17778,  0.06667, -0.26667,  0.33333)
                in M.inverse m `shouldBe` Right expected
            it "computes inverse matrix multiplying product by which returns original matrix" $
                let a = M.square4 ( 3,-9, 7, 3,
                                    3,-8, 2,-9,
                                   -4, 4, 4, 1,
                                   -6, 5,-1, 1)
                    b = M.square4 ( 8, 2, 2, 2,
                                    3,-1, 7, 0,
                                    7, 0, 5, 4,
                                    6,-2, 0, 5)
                    result = join $ M.multiply <$> a `M.multiply` b <*> M.inverse b
                in result `shouldBe` Right a