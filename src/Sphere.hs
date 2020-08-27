module Sphere
    ( Sphere (..)
    , RaySphereIntersection (..)
    , SphereIntersection (..)
    , intersect
    , sphere
    ) where

import Ray (Ray (..))
import Space (Point (..), subtractPoint, dot)

newtype Sphere = Sphere { sphereId :: Int }
    deriving (Show, Eq)

data SphereIntersection = SphereIntersection Sphere Float
    deriving (Show, Eq)

data RaySphereIntersection = Miss | Intersection SphereIntersection SphereIntersection
    deriving (Show, Eq)

sphere :: Int -> (Sphere, Int)
sphere newId = (Sphere newId, newId + 1)

intersect :: Sphere -> Ray -> RaySphereIntersection
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
                in Intersection (SphereIntersection s (min t1 t2)) (SphereIntersection s (max t1 t2))
    in result   
