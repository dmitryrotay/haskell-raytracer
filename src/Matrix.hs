module Matrix
    ( square2
    , square3
    , square4
    , vector4
    , transpose
    , minor
    , cofactor
    , determinant
    , submatrix
    , multiply
    , identity
    , invertible
    , inverse
    ) where

import Common

transposeA :: [[a]] -> [[a]]
transposeA ([]:_) = []
transposeA x = map head x : transposeA (map tail x)

newtype SquareMatrix = SquareMatrix [[Float]] deriving (Show)

instance Eq SquareMatrix where
    SquareMatrix m1 == SquareMatrix m2 = all (uncurry (~==)) (zip (concat m1) (concat m2))

transpose :: SquareMatrix -> SquareMatrix
transpose (SquareMatrix m) = SquareMatrix (transposeA m)

submatrix :: SquareMatrix -> Int -> Int -> SquareMatrix
submatrix (SquareMatrix m) removeRow removeCol = 
    let rows = zip [0..] [zip [0..] row | row <- m]
    in SquareMatrix [[col | (colIndex, col) <- row, colIndex /= removeCol] | (rowIndex, row) <- rows, rowIndex /= removeRow]

determinant :: SquareMatrix -> Float
determinant (SquareMatrix []) = 1
determinant (SquareMatrix m) = sum (zipWith (\i x -> x * cofactor (SquareMatrix m) 0 i) [0..] (head m))

minor :: SquareMatrix -> Int -> Int -> Float
minor m row col = determinant (submatrix m row col)

cofactor :: SquareMatrix -> Int -> Int -> Float
cofactor m row col = 
    let sign = if odd (row + col) then -1 else 1
    in sign * minor m row col

type SquareTuple2 = (Float, Float,
                     Float, Float)

square2 :: SquareTuple2 -> SquareMatrix
square2 (m00, m01,
         m10, m11) = SquareMatrix [[m00, m01],
                                   [m10, m11]]

type SquareTuple3 = (Float, Float, Float,
                     Float, Float, Float,
                     Float, Float, Float)

square3 :: SquareTuple3 -> SquareMatrix
square3 (m00, m01, m02,
         m10, m11, m12,
         m20, m21, m22) = SquareMatrix [[m00, m01, m02],
                                        [m10, m11, m12],
                                        [m20, m21, m22]]

type SquareTuple4 = (Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float,
                     Float, Float, Float, Float)

square4 :: SquareTuple4 -> SquareMatrix
square4 (m00, m01, m02, m03,
         m10, m11, m12, m13,
         m20, m21, m22, m23,
         m30, m31, m32, m33) = SquareMatrix [[m00, m01, m02, m03],
                                             [m10, m11, m12, m13],
                                             [m20, m21, m22, m23],
                                             [m30, m31, m32, m33]]

vector4 :: (Float, Float, Float, Float) -> SquareMatrix
vector4 (x0, x1, x2, x3) = SquareMatrix [[x0],
                                         [x1],
                                         [x2],
                                         [x3]]

cross :: [Float] -> [Float] -> Float
cross a1 a2 = sum (zipWith (*) a1 a2)

multiply :: SquareMatrix -> SquareMatrix -> Either String SquareMatrix
m1 `multiply` m2 = 
    let (SquareMatrix rows1) = m1
        (SquareMatrix cols2) = transpose m2
        product
            | length rows1 /= length (head cols2) = Left "m1 height must be equal to m2 width"
            | otherwise =
                let productArray = [[cross row1 col2 | col2 <- cols2] | row1 <- rows1]
                in Right (SquareMatrix productArray)
    in product

identity :: Int -> SquareMatrix
identity n = SquareMatrix [[if row == col then 1 else 0 | col <- [0..n-1]] | row <- [0..n-1]]

invertible :: SquareMatrix -> Bool
invertible m = determinant m /= 0

inverse :: SquareMatrix -> Either String SquareMatrix
inverse m
    | invertible m =
        let d = determinant m
            (SquareMatrix a) = m
            n = length a
            c = SquareMatrix [[cofactor m row col / d | col <- [0..n-1]] | row <- [0..n-1]]
        in Right (transpose c)
    | otherwise = Left "The given matrix is not invertible"
    
