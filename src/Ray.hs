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
    )

import Matrix (Matrix, fromPoint, fromVector, toPoint, toVector)
import Transform (Transform, translation, scaling, (|<>|))

data Ray = Ray { origin :: Point, direction :: Vector }
    deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `addVectorP` (direction `multiplyVector` t)

transform :: Ray -> Transform -> Ray
transform (Ray origin direction) transformMatrix =
    let originMatrix = fromPoint origin
        directionMatrix = fromVector direction
        originMatrix' = originMatrix |<>| transformMatrix
        directionMatrix' = directionMatrix |<>| transformMatrix
        origin' = toPoint originMatrix'
        direction' = toVector directionMatrix'
    in Ray origin' direction'
