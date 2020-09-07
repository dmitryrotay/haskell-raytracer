module Common
    ( (~==)
    , epsilon
    ) where

epsilon :: Float
epsilon = 1e-5

(~==) :: Float -> Float -> Bool
x ~== y = abs (x - y) < epsilon
