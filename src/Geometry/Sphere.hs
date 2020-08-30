module Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , intersect
    , createSphere
    , setTransform
    ) where

import Ray (Ray (..), transform)
import Space (Point (..), subtractPoint, dot)
import Geometry (Intersection (..))
import Matrix (Matrix, inverse)
import Transform (Transform, identity, (|<>|))

data Sphere = Sphere { getSphereId :: Int, getTransform :: Transform }
    deriving (Show, Eq)

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

createSphere :: Int -> (Sphere, Int)
createSphere newId = (Sphere newId identity, newId + 1)

intersect :: Sphere -> Ray -> Either String SphereRayIntersection
intersect s ray =
    do
        t' <- inverse $ getTransform s
        let sphereToRay = origin `subtractPoint` Point 0 0 0
            (Ray origin direction) = transform ray t'
            a = direction `dot` direction
            b = 2 * (direction `dot` sphereToRay)
            c = sphereToRay `dot` sphereToRay - 1
            discriminant =  b ^ 2 - 4 * a * c
            result
                | discriminant < 0 = Right Miss
                | otherwise =
                    let t1 = (-b - sqrt discriminant) / (2 * a)
                        t2 = (-b + sqrt discriminant) / (2 * a)
                        p1 = Intersection s (min t1 t2)
                        p2 = Intersection s (max t1 t2)
                    in Right $ SphereRayIntersection p1 p2
            in result   

setTransform :: Sphere -> Transform -> Sphere
setTransform (Sphere id _) = Sphere id
