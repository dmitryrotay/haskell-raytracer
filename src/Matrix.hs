module Matrix
    ( SquareMatrix2 (..)
    , SquareMatrix3 (..)
    , SquareMatrix4 (..)
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