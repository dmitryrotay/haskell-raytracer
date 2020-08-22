module Matrix
    ( SquareMatrix2 (..)
    , determinant2
    , SquareMatrix3 (..)
    , subMatrix3
    , SquareMatrix4 (..)
    , subMatrix4
    , multiply4
    , multiplyTuple4
    , identityMatrix4
    , transpose4
    ) where

import Common

newtype SquareMatrix2 = SquareMatrix2 ((Float, Float),
                                       (Float, Float)) deriving (Show)

instance Eq SquareMatrix2 where
    SquareMatrix2 m == SquareMatrix2 n =
        let ((m00, m10),
             (m01, m11)) = m
            ((n00, n10),
             (n01, n11)) = n
        in    m00 ~== n00 && m10 ~== n10
           && m01 ~== n01 && m11 ~== n11

determinant2 :: SquareMatrix2 -> Float
determinant2 (SquareMatrix2 ((m00, m10),
                            (m01, m11))) =
                                m00 * m11 - m10 * m01

newtype SquareMatrix3 = SquareMatrix3 ((Float, Float, Float),
                                       (Float, Float, Float),
                                       (Float, Float, Float)) deriving (Show)

instance Eq SquareMatrix3 where
    SquareMatrix3 m == SquareMatrix3 n =
        let ((m00, m10, m20),
             (m01, m11, m21),
             (m02, m12, m22)) = m
            ((n00, n10, n20),
             (n01, n11, n21),
             (n02, n12, n22)) = n
        in    m00 ~== n00 && m10 ~== n10 && m20 ~== n20
           && m01 ~== n01 && m11 ~== n11 && m21 ~== n21
           && m02 ~== n02 && m12 ~== n12 && m22 ~== n22

toRowsArray3 :: SquareMatrix3 -> MatrixArray
toRowsArray3 (SquareMatrix3 ((m00, m10, m20),
                             (m01, m11, m21),
                             (m02, m12, m22))) =
                               MatrixArray [[m00, m10, m20],
                                            [m01, m11, m21],
                                            [m02, m12, m22]]

subMatrix3 :: SquareMatrix3 -> Int -> Int -> SquareMatrix2
subMatrix3 m removeRow removeCol =
    let MatrixArray [[m00, m10],
                     [m01, m11]] = matrixSubarray (toRowsArray3 m) removeRow removeCol
    in SquareMatrix2 ((m00, m10),
                      (m01, m11))

newtype SquareMatrix4 = SquareMatrix4 ((Float, Float, Float, Float),
                                       (Float, Float, Float, Float),
                                       (Float, Float, Float, Float),
                                       (Float, Float, Float, Float)) deriving (Show)

instance Eq SquareMatrix4 where
    SquareMatrix4 m == SquareMatrix4 n =
        let ((m00, m10, m20, m30),
             (m01, m11, m21, m31),
             (m02, m12, m22, m32),
             (m03, m13, m23, m33)) = m
            ((n00, n10, n20, n30),
             (n01, n11, n21, n31),
             (n02, n12, n22, n32),
             (n03, n13, n23, n33)) = n
        in    m00 ~== n00 && m10 ~== n10 && m20 ~== n20 && m30 ~== n30
           && m01 ~== n01 && m11 ~== n11 && m21 ~== n21 && m31 ~== n31
           && m02 ~== n02 && m12 ~== n12 && m22 ~== n22 && m32 ~== n32
           && m03 ~== n03 && m13 ~== n13 && m23 ~== n23 && m33 ~== n33

subMatrix4 :: SquareMatrix4 -> Int -> Int -> SquareMatrix3
subMatrix4 m removeRow removeCol =
    let MatrixArray [[m00, m10, m20],
                     [m01, m11, m21],
                     [m02, m12, m22]] = matrixSubarray (toRowsArray4 m) removeRow removeCol
    in SquareMatrix3 ((m00, m10, m20),
                      (m01, m11, m21),
                      (m02, m12, m22)) 

newtype MatrixArray = MatrixArray [[Float]]

matrixSubarray :: MatrixArray -> Int -> Int -> MatrixArray
matrixSubarray (MatrixArray m) removeRow removeCol = 
    let rows = zip [0..] [zip [0..] row | row <- m]
    in MatrixArray [[col | (colIndex, col) <- row, colIndex /= removeCol] | (rowIndex, row) <- rows, rowIndex /= removeRow]

toRowsArray4 :: SquareMatrix4 -> MatrixArray
toRowsArray4 (SquareMatrix4 ((m00, m10, m20, m30),
                             (m01, m11, m21, m31),
                             (m02, m12, m22, m32),
                             (m03, m13, m23, m33))) =
                               MatrixArray [[m00, m10, m20, m30],
                                            [m01, m11, m21, m31],
                                            [m02, m12, m22, m32],
                                            [m03, m13, m23, m33]]

toColsArray4 :: SquareMatrix4 -> MatrixArray
toColsArray4 (SquareMatrix4 ((m00, m10, m20, m30),
                             (m01, m11, m21, m31),
                             (m02, m12, m22, m32),
                             (m03, m13, m23, m33))) =
                               MatrixArray [[m00, m01, m02, m03],
                                            [m10, m11, m12, m13],
                                            [m20, m21, m22, m23],
                                            [m30, m31, m32, m33]]

cross :: [Float] -> [Float] -> Float
cross a1 a2 = sum (zipWith (*) a1 a2)

multiply4 :: SquareMatrix4 -> SquareMatrix4 -> SquareMatrix4
m1 `multiply4` m2 = 
    let MatrixArray rows1 = toRowsArray4 m1
        MatrixArray cols2 = toColsArray4 m2
        productMatrixArray = [[cross row1 col2 | col2 <- cols2] | row1 <- rows1]
        [[m00, m10, m20, m30],
         [m01, m11, m21, m31],
         [m02, m12, m22, m32],
         [m03, m13, m23, m33]] = productMatrixArray
    in SquareMatrix4 ((m00, m10, m20, m30),
                      (m01, m11, m21, m31),
                      (m02, m12, m22, m32),
                      (m03, m13, m23, m33))

type Tuple4 = (Float, Float, Float, Float)

multiplyTuple4 :: SquareMatrix4 -> Tuple4 -> Tuple4
multiplyTuple4 m (t0, t1, t2, t3) =
    let MatrixArray [row0, row1, row2, row3] = toRowsArray4 m
        a = [t0, t1, t2, t3]
    in (cross row0 a, cross row1 a, cross row2 a, cross row3 a)

identityMatrix4 :: SquareMatrix4
identityMatrix4 = SquareMatrix4 ((1, 0, 0, 0),
                                 (0, 1, 0, 0),
                                 (0, 0, 1, 0),
                                 (0, 0, 0, 1))

transpose4 :: SquareMatrix4 -> SquareMatrix4
transpose4 (SquareMatrix4 ((m00, m10, m20, m30),
                           (m01, m11, m21, m31),
                           (m02, m12, m22, m32),
                           (m03, m13, m23, m33))) =
                               SquareMatrix4 ((m00, m01, m02, m03),
                                              (m10, m11, m12, m13),
                                              (m20, m21, m22, m23),
                                              (m30, m31, m32, m33))