module RayTracing
    ( Ray (..)
    , position
    ) where

import Space
    ( Point (..)
    , Vector (..)
    , addVectorP
    , multiplyVector
    )

data Ray = Ray { origin :: Point, direction :: Vector }

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `addVectorP` (direction `multiplyVector` t)

