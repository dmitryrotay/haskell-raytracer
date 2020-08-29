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
import Transform (translation, scaling, (|<>|))

data Ray = Ray { origin :: Point, direction :: Vector }
    deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `addVectorP` (direction `multiplyVector` t)

transform :: Ray -> Matrix -> Either String Ray
transform (Ray origin direction) transformMatrix =
    let originMatrix = fromPoint origin
        directionMatrix = fromVector direction
    in do
        originMatrix' <- originMatrix |<>| transformMatrix
        directionMatrix' <- directionMatrix |<>| transformMatrix
        origin' <- toPoint originMatrix'
        direction' <- toVector directionMatrix'
        Right $ Ray origin' direction'
