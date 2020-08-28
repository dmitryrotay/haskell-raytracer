module Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , intersect
    , sphere
    ) where

import Ray (Ray (..))
import Space (Point (..), subtractPoint, dot)
import Geometry (Intersection (..))

newtype Sphere = Sphere { sphereId :: Int }
    deriving (Show, Eq)

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

sphere :: Int -> (Sphere, Int)
sphere newId = (Sphere newId, newId + 1)

intersect :: Sphere -> Ray -> SphereRayIntersection
intersect s (Ray origin direction) =
    let sphereToRay = origin `subtractPoint` Point 0 0 0
        a = direction `dot` direction
        b = 2 * (direction `dot` sphereToRay)
        c = sphereToRay `dot` sphereToRay - 1
        discriminant =  b ^ 2 - 4 * a * c
        result
            | discriminant < 0 = Miss
            | otherwise =
                let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                    p1 = Intersection s (min t1 t2)
                    p2 = Intersection s (max t1 t2)
                in SphereRayIntersection p1 p2
    in result   
