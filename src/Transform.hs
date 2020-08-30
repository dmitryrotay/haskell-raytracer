{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}

module Transform 
    ( Transform
    , translation
    , scaling
    , rotationX
    , rotationY
    , rotationZ
    , shearing
    , combine
    , identity
    , (|<>|)
    ) where

import           Matrix (Matrix (..), SquareMatrix, (|*|), square4)
import           GHC.TypeNats
import qualified Matrix as M (identity)

(|<>|) :: Matrix 4 w -> Transform -> Matrix 4 w
(|<>|) = flip (|*|)

type Transform = SquareMatrix 4

identity :: Transform
identity = M.identity :: Transform

combine :: [Transform] -> Transform
combine = foldr (|*|) identity

translation :: Float -> Float -> Float -> SquareMatrix 4
translation x y z = square4 (1, 0, 0, x,
                             0, 1, 0, y,
                             0, 0, 1, z,
                             0, 0, 0, 1)

scaling :: Float -> Float -> Float -> SquareMatrix 4
scaling x y z = square4 (x, 0, 0, 0,
                         0, y, 0, 0,
                         0, 0, z, 0,
                         0, 0, 0, 1)

rotationX :: Float -> SquareMatrix 4
rotationX r = square4 (1,    0,     0,      0,
                       0,    cos r, -sin r, 0,
                       0,    sin r, cos r,  0,
                       0,    0,     0,      1)

rotationY :: Float -> SquareMatrix 4
rotationY r = square4 (cos r,    0,  sin r,  0,
                           0,    1,      0,  0,
                       -sin r,   0,  cos r,  0,
                           0,    0,      0,  1)

rotationZ :: Float -> SquareMatrix 4
rotationZ r = square4 (cos r,  -sin r,   0,  0,
                       sin r,   cos r,   0,  0,
                           0,       0,   1,  0,
                           0,       0,   0,  1)

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> SquareMatrix 4
shearing xy xz yx yz zx zy = square4 ( 1,  xy, xz, 0,
                                      yx,   1, yz, 0,
                                      zx,  zy,  1, 0,
                                       0,   0,  0, 1)
