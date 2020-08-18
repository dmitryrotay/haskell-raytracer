module Common
    ( (~==)
    ) where

(~==) :: Float -> Float -> Bool
x ~== y = abs (x - y) < 1e-5

