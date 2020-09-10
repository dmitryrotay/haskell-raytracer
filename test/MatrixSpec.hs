{-# LANGUAGE DataKinds #-}

module MatrixSpec where

import Matrix
    ( (|*|)
    , SquareMatrix
    , identity
    , square2
    , square3
    , square4
    , fromTuple4
    , transpose
    , inverse
    )

import Test.Hspec

spec :: Spec
spec = do
    describe "Matrix" $ do
        describe "SquareMatrix" $ do
            it "uses floating point equality comparison" $
                let m1 = square2 (10.2 ** 2, 5.2 ** 2,
                                  10.2 ** 2, 5.2 ** 2)
                    m2 = square2 (104.04, 27.04,
                                  104.04, 27.04)
                in m1 `shouldBe` m2
       
        describe "multiply" $ do
            it "multiplies two matrices returning valid product matrix" $
                let m1 = square4 (1, 2, 3, 4,
                                  5, 6, 7, 8,
                                  9, 8, 7, 6,
                                  5, 4, 3, 2)
                    m2 = square4 (-2, 1, 2, 3,
                                   3, 2, 1, -1,
                                   4, 3, 6, 5,
                                   1, 2, 7, 8)
                    expected = square4 (20, 22, 50, 48,
                                        44, 54, 114, 108,
                                        40, 58, 110, 102,
                                        16, 26, 46, 42)
                in m1 |*| m2 `shouldBe` expected
            it "multiplies by tuple returning valid product tuple" $
                let m = square4 (1, 2, 3, 4,
                                 2, 4, 4, 2,
                                 8, 6, 4, 1,
                                 0, 0, 0, 1)
                    t = fromTuple4 (1, 2, 3, 1)
                    expected = fromTuple4 (18, 24, 33, 1)
                in m |*| t `shouldBe` expected
            it "multiplies by identity matrix returning original matrix" $
                let m = square4 (0, 1, 2, 4,
                                 1, 2, 4, 8,
                                 2, 4, 8, 16,
                                 4, 8, 16, 32)
                    identityMatrix = identity :: SquareMatrix 4
                in m |*| identityMatrix `shouldBe`  m
                    
        describe "transpose" $ do
            it "produces matrix with swapped rows and columns" $
                let m = square4 (0, 9, 3, 0,
                                 9, 8, 0, 8,
                                 1, 8, 5, 3,
                                 0, 0, 5, 8)
                    expected = square4 (0, 9, 1, 0,
                                        9, 8, 8, 0,
                                        3, 0, 5, 5,
                                        0, 8, 3, 8)
                in transpose m `shouldBe` expected
            it "produces identity matrix from identity matrix" $
                let identityMatrix = identity :: SquareMatrix 4
                in transpose identityMatrix `shouldBe` identityMatrix
               
        describe "inverse" $ do
            it "computes inverse matrix correctly 1" $
                let m = square4 (-5, 2, 6,-8,
                                  1,-5, 1, 8,
                                  7, 7,-6,-7,
                                  1,-3, 7, 4)
                    expected = square4 ( 0.21805,  0.45113,  0.24060, -0.04511,
                                        -0.80827, -1.45677, -0.44361,  0.52068,
                                        -0.07895, -0.22368, -0.05263,  0.19737,
                                        -0.52256, -0.81391, -0.30075,  0.30639)
                in inverse m `shouldBe` expected
            it "computes inverse matrix correctly 2" $
                let m = square4 ( 8,-5, 9, 2,
                                  7, 5, 6, 1,
                                 -6, 0, 9, 6,
                                 -3, 0,-9,-4)
                    expected = square4 (-0.15385, -0.15385, -0.28205, -0.53846,
                                        -0.07692,  0.12308,  0.02564,  0.03077,
                                         0.35897,  0.35897,  0.43590,  0.92308,
                                        -0.69231, -0.69231,  -0.76923, -1.92308)
                in inverse m `shouldBe` expected
            it "computes inverse matrix correctly 3" $
                let m = square4 ( 9, 3, 0, 9,
                                 -5,-2,-6,-3,
                                 -4, 9, 6, 4,
                                 -7, 6, 6, 2)
                    expected = square4 (-0.04074, -0.07778,  0.14444, -0.22222,
                                        -0.07778,  0.03333,  0.36667, -0.33333,
                                        -0.02901, -0.14630, -0.10926,  0.12963,
                                         0.17778,  0.06667, -0.26667,  0.33333)
                in inverse m `shouldBe` expected
            it "multiplying product by inverse matrix returns original matrix" $
                let a = square4 ( 3,-9, 7, 3,
                                  3,-8, 2,-9,
                                 -4, 4, 4, 1,
                                 -6, 5,-1, 1)
                    b = square4 ( 8, 2, 2, 2,
                                  3,-1, 7, 0,
                                  7, 0, 5, 4,
                                  6,-2, 0, 5)
                    productMatrix = a |*| b
                    result = productMatrix |*| inverse b
                in result `shouldBe` a
