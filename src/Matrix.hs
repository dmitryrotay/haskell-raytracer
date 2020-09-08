{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Matrix
    ( SpaceMatrix
    , SquareMatrix
    , square2
    , square3
    , square4
    , fromTuple4
    , transpose
    , minor
    , cofactor
    , determinant
    , submatrix
    , (|*|)
    , identity
    , invertible
    , inverse
    , toSpaceCoordinates
    ) where

import Common ((~==))
import Control.Exception (throw, ArithException (..), Exception)
import Data.Proxy
import GHC.TypeNats

transposeA :: [[a]] -> [[a]]
transposeA ([]:_) = []
transposeA x = map head x : transposeA (map tail x)

newtype Matrix (height :: Nat) (width :: Nat) = Matrix [[Double]]
    deriving Show

type SpaceMatrix width = Matrix 4 width

type SquareMatrix n = Matrix n n

instance Eq (Matrix h w) where
    Matrix m1 == Matrix m2 = all (uncurry (~==)) (zip (concat m1) (concat m2))

transpose :: Matrix h w -> Matrix w h
transpose (Matrix m) = Matrix (transposeA m)

submatrix :: Matrix h w -> Int -> Int -> Matrix (h - 1) (w - 1)
submatrix (Matrix m) removeRow removeCol = 
    let rows = zip [0..] [zip [0..] row | row <- m]
    in Matrix [
                [col | (colIndex, col) <- row, colIndex /= removeCol]
                | (rowIndex, row) <- rows, rowIndex /= removeRow
              ]

determinant :: Matrix h w -> Double
determinant (Matrix []) = 1
determinant matrix =
    let (Matrix m) = matrix
    in sum (zipWith (\i x -> x * cofactor matrix 0 i) [0..] (head m))

minor :: Matrix h w -> Int -> Int -> Double
minor matrix row col = determinant (submatrix matrix row col)

cofactor :: Matrix h w -> Int -> Int -> Double
cofactor matrix row col = 
    let sign = if odd (row + col) then -1 else 1
    in sign * minor matrix row col

type SquareTuple2 = (Double, Double,
                     Double, Double)

square2 :: SquareTuple2 -> SquareMatrix 2
square2 (m00, m01,
         m10, m11) = Matrix [[m00, m01],
                             [m10, m11]]

type SquareTuple3 = (Double, Double, Double,
                     Double, Double, Double,
                     Double, Double, Double)

square3 :: SquareTuple3 -> SquareMatrix 3
square3 (m00, m01, m02,
         m10, m11, m12,
         m20, m21, m22) = Matrix [[m00, m01, m02],
                                  [m10, m11, m12],
                                  [m20, m21, m22]]

type SquareTuple4 = (Double, Double, Double, Double,
                     Double, Double, Double, Double,
                     Double, Double, Double, Double,
                     Double, Double, Double, Double)

square4 :: SquareTuple4 -> SquareMatrix 4
square4 (m00, m01, m02, m03,
         m10, m11, m12, m13,
         m20, m21, m22, m23,
         m30, m31, m32, m33) = Matrix [[m00, m01, m02, m03],
                                       [m10, m11, m12, m13],
                                       [m20, m21, m22, m23],
                                       [m30, m31, m32, m33]]

fromTuple4 :: (Double, Double, Double, Double) -> Matrix 4 1
fromTuple4 (x0, x1, x2, x3) = Matrix [[x0],
                                      [x1],
                                      [x2],
                                      [x3]]

cross :: [Double] -> [Double] -> Double
cross a1 a2 = sum (zipWith (*) a1 a2)

(|*|) :: Matrix h1 w1 -> Matrix w1 h2 -> Matrix h1 h2
matrix1 |*| matrix2 = 
    let (Matrix rows1) = matrix1
        (Matrix cols2) = transpose matrix2
        productArray = [[cross row1 col2 | col2 <- cols2] | row1 <- rows1]
    in Matrix productArray

identity :: forall n. KnownNat n => SquareMatrix n
identity =
    let size = natVal (Proxy @n)
    in Matrix [[if row == col then 1 else 0 | col <- [0..size - 1]] | row <- [0..size - 1]]

invertible :: Matrix h w -> Bool
invertible matrix = determinant matrix /= 0

inverse :: Matrix h w -> Matrix h w
inverse matrix
    | invertible matrix =
        let d = determinant matrix
            (Matrix m) = matrix
            n = length m
            c = Matrix [[cofactor matrix row col / d | col <- [0..n - 1]] | row <- [0..n - 1]]
        in transpose c
    | otherwise = throw DivideByZero

data InvalidSpaceMatrixException = InvalidSpaceMatrixException deriving Show
instance Exception InvalidSpaceMatrixException

toSpaceCoordinates :: SpaceMatrix 1 -> (Double, Double, Double)
toSpaceCoordinates (Matrix [[x], [y], [z], [_]]) = (x, y, z)
toSpaceCoordinates _ = throw InvalidSpaceMatrixException