{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Transform 
    ( Transform
    , translation
    , scaling
    , rotationX
    , rotationY
    , rotationZ
    , shearing
    , combine
    , identity
    , transformPoint
    , transformVector
    , viewTransform
    , (|<>|)
    ) where

import           Matrix (SpaceMatrix, (|*|), square4, toSpaceCoordinates)
import qualified Matrix as M (identity)
import           Space
    ( Point (..)
    , Vector (..)
    , vectorToMatrix
    , pointToMatrix
    , cross
    , normalize
    , subtractPoint
    )

(|<>|) :: SpaceMatrix width -> Transform -> SpaceMatrix width
(|<>|) = flip (|*|)

type Transform = SpaceMatrix 4

identity :: Transform
identity = M.identity :: Transform

combine :: [Transform] -> Transform
combine = foldr (|*|) identity

translation :: Float -> Float -> Float -> Transform
translation x y z = square4 (1, 0, 0, x,
                             0, 1, 0, y,
                             0, 0, 1, z,
                             0, 0, 0, 1)

scaling :: Float -> Float -> Float -> Transform
scaling x y z = square4 (x, 0, 0, 0,
                         0, y, 0, 0,
                         0, 0, z, 0,
                         0, 0, 0, 1)

rotationX :: Float -> Transform
rotationX r = square4 (1,    0,     0,      0,
                       0,    cos r, -sin r, 0,
                       0,    sin r, cos r,  0,
                       0,    0,     0,      1)

rotationY :: Float -> Transform
rotationY r = square4 (cos r,    0,  sin r,  0,
                           0,    1,      0,  0,
                       -sin r,   0,  cos r,  0,
                           0,    0,      0,  1)

rotationZ :: Float -> Transform
rotationZ r = square4 (cos r,  -sin r,   0,  0,
                       sin r,   cos r,   0,  0,
                           0,       0,   1,  0,
                           0,       0,   0,  1)

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> Transform
shearing xy xz yx yz zx zy = square4 ( 1,  xy, xz, 0,
                                      yx,   1, yz, 0,
                                      zx,  zy,  1, 0,
                                       0,   0,  0, 1)

transformPoint :: Point -> Transform -> Point
transformPoint p t =
    let matrix = pointToMatrix p
        transformedMatrix = matrix |<>| t
        (x, y, z) = toSpaceCoordinates transformedMatrix
    in Point x y z

transformVector :: Vector -> Transform -> Vector
transformVector v t =
    let matrix = vectorToMatrix v
        transformedMatrix = matrix |<>| t
        (x, y, z) = toSpaceCoordinates transformedMatrix
    in Vector x y z

viewTransform :: Point -> Point -> Vector -> Transform
viewTransform from to up =
    let forward = normalize (to `subtractPoint` from)
        upn = normalize up
        left = forward `cross` upn
        trueUp = left `cross` forward
        (Vector forwardX forwardY forwardZ) = forward
        (Vector upX upY upZ) = trueUp
        (Vector leftX leftY leftZ) = left
        orientation = square4 (     leftX,     leftY,     leftZ, 0,
                                      upX,       upY,       upZ, 0,
                                -forwardX, -forwardY, -forwardZ, 0,
                                        0,         0,         0, 1
                              )
        (Point fromX fromY fromZ) = from
    in orientation |*| translation (-fromX) (-fromY) (-fromZ)