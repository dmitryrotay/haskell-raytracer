module Transform 
    ( translation
    ) where

import qualified Matrix as M

translation :: Float -> Float -> Float -> M.Matrix
translation x y z = M.square4 (1, 0, 0, x,
                               0, 1, 0, y,
                               0, 0, 1, z,
                               0, 0, 0, 1)
