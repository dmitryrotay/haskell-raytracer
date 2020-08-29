module Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , intersect
    , sphere
    , setTransform
    ) where

import Ray (Ray (..), transform)
import Space (Point (..), subtractPoint, dot)
import Geometry (Intersection (..))
import Matrix (Matrix, inverse)
import Transform (identity, (|<>|))

data Sphere = Sphere { getSphereId :: Int, getTransform :: Matrix }
    deriving (Show, Eq)

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

sphere :: Int -> (Sphere, Int)
sphere newId = (Sphere newId identity, newId + 1)

intersect :: Sphere -> Ray -> Either String SphereRayIntersection
intersect s ray =
    do
        t' <- inverse $ getTransform s
        (Ray origin direction) <- transform ray t'
        let sphereToRay = origin `subtractPoint` Point 0 0 0
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

setTransform :: Sphere -> Matrix -> Sphere
setTransform (Sphere id _) = Sphere id
