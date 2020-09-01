module Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , intersect
    , createSphere
    , setTransform
    , normalAt
    ) where

import Ray (Ray (..), transform)
import Space 
    ( Point (..)
    , Vector(..)
    , subtractPoint
    , dot
    , normalize
    , transformPoint
    , transformVector)
import Geometry (Intersection (..))
import Matrix (inverse, transpose)
import Transform (Transform, identity, (|<>|))

data Sphere = Sphere { getSphereId :: Int, getTransform :: Transform }
    deriving (Show, Eq)

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

createSphere :: Int -> (Sphere, Int)
createSphere newId = (Sphere newId identity, newId + 1)

intersect :: Sphere -> Ray -> SphereRayIntersection
intersect sphere ray =
    let sphereToRay = origin `subtractPoint` Point 0 0 0
        inverseTransform = inverse . getTransform $ sphere --possible performance hit
        (Ray origin direction) = transform ray inverseTransform
        a = direction `dot` direction
        b = 2 * (direction `dot` sphereToRay)
        c = sphereToRay `dot` sphereToRay - 1
        discriminant =  b ^ 2 - 4 * a * c
        result
            | discriminant < 0 = Miss
            | otherwise =
                let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                    p1 = Intersection sphere (min t1 t2)
                    p2 = Intersection sphere (max t1 t2)
                in SphereRayIntersection p1 p2
        in result   

setTransform :: Sphere -> Transform -> Sphere
setTransform (Sphere id _) = Sphere id

normalAt :: Sphere -> Point -> Vector
normalAt sphere point =
    let objectPoint = transformPoint point (inverse (getTransform sphere))
        objectNormal = objectPoint `subtractPoint` Point 0 0 0
        worldNormal = transformVector
                      objectNormal
                      (transpose . inverse . getTransform $ sphere)
    in normalize worldNormal