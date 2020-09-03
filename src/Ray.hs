module Ray
    ( Ray (..)
    , position
    , transformRay
    ) where

import Space
    ( Point (..)
    , Vector (..)
    , addVectorP
    , multiplyVector
    , transformPoint
    , transformVector
    )

import Transform (Transform)

data Ray = Ray { getOrigin :: Point, getDirection :: Vector }
    deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `addVectorP` (direction `multiplyVector` t)

transformRay :: Ray -> Transform -> Ray
transformRay (Ray origin direction) transformMatrix =
    let transformedOrigin = transformPoint origin transformMatrix
        transformedDirection = transformVector direction transformMatrix
    in Ray transformedOrigin transformedDirection
