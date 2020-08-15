module Common
    ( floatEq
    ) where

floatEq :: Float -> Float -> Bool
x `floatEq` y = abs (x - y) < 1e-5

