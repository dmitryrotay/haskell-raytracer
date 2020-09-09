{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Matrix
    ( SpaceMatrix
    , SquareMatrix
    , square2
    , square3
    , square4
    , fromTuple4
    , transpose
    , (|*|)
    , identity
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

cofactor :: Int -> Int ->
            Double -> Double -> Double ->
            Double -> Double -> Double ->
            Double -> Double -> Double ->
            Double
cofactor row col
         m01 m02 m03
         m11 m12 m13
         m21 m22 m23 =
    let minor01 = m12 * m23 - m13 * m22
        minor02 = m11 * m23 - m13 * m21
        minor03 = m11 * m22 - m12 * m21
        sign = if odd (row + col) then -1 else 1
    in sign * (m01 * minor01 - m02 * minor02 + m03 * minor03)

data InvalidMatrixException = InvalidMatrixException deriving Show
instance Exception InvalidMatrixException

inverse :: SquareMatrix 4 -> SquareMatrix 4
inverse (Matrix [[m00, m01, m02, m03],
                 [m10, m11, m12, m13],
                 [m20, m21, m22, m23],
                 [m30, m31, m32, m33]]) =
    let c00 = cofactor 0 0
                       m11 m12 m13
                       m21 m22 m23
                       m31 m32 m33
        
        c01 = cofactor 0 1
                       m10 m12 m13
                       m20 m22 m23
                       m30 m32 m33
        
        c02 = cofactor 0 2
                       m10 m11 m13
                       m20 m21 m23
                       m30 m31 m33

        c03 = cofactor 0 3
                       m10 m11 m12
                       m20 m21 m22
                       m30 m31 m32

        c10 = cofactor 1 0
                       m01 m02 m03
                       m21 m22 m23
                       m31 m32 m33
        
        c11 = cofactor 1 1
                       m00 m02 m03
                       m20 m22 m23
                       m30 m32 m33
        
        c12 = cofactor 1 2
                       m00 m01 m03
                       m20 m21 m23
                       m30 m31 m33

        c13 = cofactor 1 3
                       m00 m01 m02
                       m20 m21 m22
                       m30 m31 m32

        c20 = cofactor 2 0
                       m01 m02 m03
                       m11 m12 m13
                       m31 m32 m33
        
        c21 = cofactor 2 1
                       m00 m02 m03
                       m10 m12 m13
                       m30 m32 m33
        
        c22 = cofactor 2 2
                       m00 m01 m03
                       m10 m11 m13
                       m30 m31 m33

        c23 = cofactor 2 3
                       m00 m01 m02
                       m10 m11 m12
                       m30 m31 m32

        c30 = cofactor 3 0
                       m01 m02 m03
                       m11 m12 m13
                       m21 m22 m23
        
        c31 = cofactor 3 1
                       m00 m02 m03
                       m10 m12 m13
                       m20 m22 m23
        
        c32 = cofactor 3 2
                       m00 m01 m03
                       m10 m11 m13
                       m20 m21 m23

        c33 = cofactor 3 3
                       m00 m01 m02
                       m10 m11 m12
                       m20 m21 m22

        d = m00 * c00 + m01 * c01 + m02 * c02 + m03 * c03
    in if d /= 0 then Matrix [[c00 / d, c10 / d, c20 / d, c30 / d]
                             ,[c01 / d, c11 / d, c21 / d, c31 / d]
                             ,[c02 / d, c12 / d, c22 / d, c32 / d]
                             ,[c03 / d, c13 / d, c23 / d, c33 / d]
                             ]
       else throw DivideByZero
inverse _ = throw InvalidMatrixException

toSpaceCoordinates :: SpaceMatrix 1 -> (Double, Double, Double)
toSpaceCoordinates (Matrix [[x], [y], [z], [_]]) = (x, y, z)
toSpaceCoordinates _ = throw InvalidMatrixException