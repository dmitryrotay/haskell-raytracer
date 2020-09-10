module Common
    ( (~==)
    , epsilon
    ) where

epsilon :: Double
epsilon = 1e-5

(~==) :: Double -> Double -> Bool
x ~== y = abs (x - y) < epsilon
