{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Matrix
    ( Matrix
    , SquareMatrix
    , square2
    , square3
    , square4
    , fromTuple4
    , fromVector
    , fromPoint
    , transpose
    , minor
    , cofactor
    , determinant
    , submatrix
    , (|*|)
    , identity
    , invertible
    , inverse
    , toPoint
    , toVector
    ) where

import Common ((~==))
import Data.Proxy
import Space (Point (..), Vector (..))
import GHC.TypeNats

transposeA :: [[a]] -> [[a]]
transposeA ([]:_) = []
transposeA x = map head x : transposeA (map tail x)

newtype Matrix (width :: Nat) (height :: Nat) = Matrix [[Float]]
    deriving Show

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

determinant :: Matrix h w -> Float
determinant (Matrix []) = 1
determinant matrix =
    let (Matrix m) = matrix
    in sum (zipWith (\i x -> x * cofactor matrix 0 i) [0..] (head m))

minor :: Matrix h w -> Int -> Int -> Float
minor matrix row col = determinant (submatrix matrix row col)

cofactor :: Matrix h w -> Int -> Int -> Float
cofactor matrix row col = 
    let sign = if odd (row + col) then -1 else 1
    in sign * minor matrix row col

type SquareTuple2 = (Float, Float,
                     Float, Float)

square2 :: SquareTuple2 -> SquareMatrix 2
square2 (m00, m01,
         m10, m11) = Matrix [[m00, m01],
                             [m10, m11]]

type SquareTuple3 = (Float, Float, Float,
                     Float, Float, Float,
                     Float, Float, Float)

square3 :: SquareTuple3 -> SquareMatrix 3
square3 (m00, m01, m02,
         m10, m11, m12,
         m20, m21, m22) = Matrix [[m00, m01, m02],
                                  [m10, m11, m12],
                                  [m20, m21, m22]]

type SquareTuple4 = (Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float)

square4 :: SquareTuple4 -> SquareMatrix 4
square4 (m00, m01, m02, m03,
         m10, m11, m12, m13,
         m20, m21, m22, m23,
         m30, m31, m32, m33) = Matrix [[m00, m01, m02, m03],
                                       [m10, m11, m12, m13],
                                       [m20, m21, m22, m23],
                                       [m30, m31, m32, m33]]

fromTuple4 :: (Float, Float, Float, Float) -> Matrix 4 1
fromTuple4 (x0, x1, x2, x3) = Matrix [[x0],
                                      [x1],
                                      [x2],
                                      [x3]]

fromVector :: Vector -> Matrix 4 1
fromVector (Vector x y z) = fromTuple4 (x, y, z, 0)

toVector :: Matrix 4 1 -> Vector
toVector (Matrix [[x],[y],[z],[0]]) = Vector x y z

fromPoint :: Point -> Matrix 4 1
fromPoint (Point x y z) = fromTuple4 (x, y, z, 1)

toPoint :: Matrix 4 1 -> Point
toPoint (Matrix [[x],[y],[z],[1]]) = Point x y z

cross :: [Float] -> [Float] -> Float
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
    in Matrix [[if row == col then 1 else 0 | col <- [0..size-1]] | row <- [0..size-1]]

invertible :: Matrix h w -> Bool
invertible matrix = determinant matrix /= 0

inverse :: Matrix h w -> Either String (Matrix h w)
inverse matrix
    | invertible matrix =
        let d = determinant matrix
            (Matrix m) = matrix
            n = length m
            c = Matrix [[cofactor matrix row col / d | col <- [0..n-1]] | row <- [0..n-1]]
        in Right (transpose c)
    | otherwise = Left "The given matrix is not invertible"
