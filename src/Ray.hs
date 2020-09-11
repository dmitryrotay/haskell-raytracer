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
    )

import Transform (Transform, transformPoint, transformVector)

data Ray = Ray { getOrigin :: Point, getDirection :: Vector }
    deriving (Show, Eq)

position :: Ray -> Double -> Point
position (Ray origin direction) = addVectorP origin . multiplyVector direction

transformRay :: Ray -> Transform -> Ray
transformRay (Ray origin direction) transformMatrix =
    let transformedOrigin = transformPoint origin transformMatrix
        transformedDirection = transformVector direction transformMatrix
    in Ray transformedOrigin transformedDirection
