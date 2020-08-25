module Transform 
    ( translation
    , scaling
    , rotationX
    , rotationY
    , rotationZ
    , shearing
    , combine
    , (|<|)
    ) where

import qualified Matrix as M
import           Matrix ((|*|))

(|<|) :: M.Matrix -> M.Matrix -> Either String M.Matrix
(|<|) = flip (|*|)

noop :: M.Matrix
noop = M.identity 4

combine :: [M.Matrix] -> Either String M.Matrix
combine = foldr (\x t -> t >>= (x |<|)) (Right noop)

translation :: Float -> Float -> Float -> M.Matrix
translation x y z = M.square4 (1, 0, 0, x,
                               0, 1, 0, y,
                               0, 0, 1, z,
                               0, 0, 0, 1)

scaling :: Float -> Float -> Float -> M.Matrix
scaling x y z = M.square4 (x, 0, 0, 0,
                           0, y, 0, 0,
                           0, 0, z, 0,
                           0, 0, 0, 1)

rotationX :: Float -> M.Matrix
rotationX r = M.square4 (1,    0,     0,      0,
                         0,    cos r, -sin r, 0,
                         0,    sin r, cos r,  0,
                         0,    0,     0,      1)

rotationY :: Float -> M.Matrix
rotationY r = M.square4 (cos r,    0,  sin r,  0,
                         0,        1,      0,  0,
                         -sin r,   0,  cos r,  0,
                         0,        0,      0,  1)

rotationZ :: Float -> M.Matrix
rotationZ r = M.square4 (cos r,  -sin r,   0,  0,
                         sin r,   cos r,   0,  0,
                             0,       0,   1,  0,
                             0,       0,   0,  1)

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> M.Matrix
shearing xy xz yx yz zx zy = M.square4 (1,  xy, xz, 0,
                                        yx,  1, yz, 0,
                                        zx, zy,  1, 0,
                                         0,  0,  0, 1)