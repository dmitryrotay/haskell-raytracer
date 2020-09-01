module Ray
    ( Ray (..)
    , position
    , transform
    ) where

import Space
    ( Point (..)
    , Vector (..)
    , addVectorP
    , multiplyVector
    , transformPoint
    , transformVector
    )

import Transform (Transform, translation, scaling, (|<>|))

data Ray = Ray { getOrigin :: Point, getDirection :: Vector }
    deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `addVectorP` (direction `multiplyVector` t)

transform :: Ray -> Transform -> Ray
transform (Ray origin direction) transformMatrix =
    let transformedOrigin = transformPoint origin transformMatrix
        transformedDirection = transformVector direction transformMatrix
    in Ray transformedOrigin transformedDirection
