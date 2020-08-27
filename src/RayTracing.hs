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
position (Ray o d) t = o `addVectorP` (d `multiplyVector` t)

