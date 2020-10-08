module Common
    ( (~==)
    , average
    , epsilon
    ) where

import Data.List (genericLength)

epsilon :: Double
epsilon = 1e-4

(~==) :: Double -> Double -> Bool
x ~== y = abs (x - y) < epsilon

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs
