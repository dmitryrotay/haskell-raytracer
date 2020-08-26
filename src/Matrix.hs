module Matrix
    ( Matrix
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
    ) where

import Common
import qualified Space as S

transposeA :: [[a]] -> [[a]]
transposeA ([]:_) = []
transposeA x = map head x : transposeA (map tail x)

newtype Matrix = Matrix [[Float]] deriving (Show)

instance Eq Matrix where
    Matrix m1 == Matrix m2 = all (uncurry (~==)) (zip (concat m1) (concat m2))

transpose :: Matrix -> Matrix
transpose (Matrix m) = Matrix (transposeA m)

submatrix :: Matrix -> Int -> Int -> Matrix
submatrix (Matrix m) removeRow removeCol = 
    let rows = zip [0..] [zip [0..] row | row <- m]
    in Matrix [[col | (colIndex, col) <- row, colIndex /= removeCol] | (rowIndex, row) <- rows, rowIndex /= removeRow]

determinant :: Matrix -> Float
determinant (Matrix []) = 1
determinant (Matrix m) = sum (zipWith (\i x -> x * cofactor (Matrix m) 0 i) [0..] (head m))

minor :: Matrix -> Int -> Int -> Float
minor m row col = determinant (submatrix m row col)

cofactor :: Matrix -> Int -> Int -> Float
cofactor m row col = 
    let sign = if odd (row + col) then -1 else 1
    in sign * minor m row col

type SquareTuple2 = (Float, Float,
                     Float, Float)

square2 :: SquareTuple2 -> Matrix
square2 (m00, m01,
         m10, m11) = Matrix [[m00, m01],
                             [m10, m11]]

type SquareTuple3 = (Float, Float, Float,
                     Float, Float, Float,
                     Float, Float, Float)

square3 :: SquareTuple3 -> Matrix
square3 (m00, m01, m02,
         m10, m11, m12,
         m20, m21, m22) = Matrix [[m00, m01, m02],
                                  [m10, m11, m12],
                                  [m20, m21, m22]]

type SquareTuple4 = (Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float)

square4 :: SquareTuple4 -> Matrix
square4 (m00, m01, m02, m03,
         m10, m11, m12, m13,
         m20, m21, m22, m23,
         m30, m31, m32, m33) = Matrix [[m00, m01, m02, m03],
                                       [m10, m11, m12, m13],
                                       [m20, m21, m22, m23],
                                       [m30, m31, m32, m33]]

fromTuple4 :: (Float, Float, Float, Float) -> Matrix
fromTuple4 (x0, x1, x2, x3) = Matrix [[x0],
                                      [x1],
                                      [x2],
                                      [x3]]

fromVector :: S.Vector -> Matrix
fromVector (S.Vector x y z) = fromTuple4 (x, y, z, 0)

fromPoint :: S.Point -> Matrix 
fromPoint (S.Point x y z) = fromTuple4 (x, y, z, 1)

toPoint :: Matrix -> Either String S.Point
toPoint (Matrix [[x],[y],[z],[1]]) = Right (S.Point x y z)
toPoint _ = Left "The matrix doesn't match Point representation"

cross :: [Float] -> [Float] -> Float
cross a1 a2 = sum (zipWith (*) a1 a2)

(|*|) :: Matrix -> Matrix -> Either String Matrix
m1 |*| m2 = 
    let (Matrix rows1) = m1
        (Matrix cols2) = transpose m2
        matrixProduct
            | length rows1 /= length (head cols2) = Left "m1 height must be equal to m2 width"
            | otherwise =
                let productArray = [[cross row1 col2 | col2 <- cols2] | row1 <- rows1]
                in Right (Matrix productArray)
    in matrixProduct

identity :: Int -> Matrix
identity n = Matrix [[if row == col then 1 else 0 | col <- [0..n-1]] | row <- [0..n-1]]

invertible :: Matrix -> Bool
invertible m = determinant m /= 0

inverse :: Matrix -> Either String Matrix
inverse m
    | invertible m =
        let d = determinant m
            (Matrix a) = m
            n = length a
            c = Matrix [[cofactor m row col / d | col <- [0..n-1]] | row <- [0..n-1]]
        in Right (transpose c)
    | otherwise = Left "The given matrix is not invertible"
