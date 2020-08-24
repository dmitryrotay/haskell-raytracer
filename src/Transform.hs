module Transform 
    ( translation
    , scaling
    , rotationX
    , rotationY
    , rotationZ
    ) where

import qualified Matrix as M

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