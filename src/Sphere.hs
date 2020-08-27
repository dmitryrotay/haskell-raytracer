module Sphere
    ( Sphere (..)
    , Intersection (..)
    , intersect
    , sphere
    ) where

import Ray (Ray (..))
import Space (Point (..), subtractPoint, dot)

newtype Sphere = Sphere { id :: Int }

data Intersection = Miss | Hit Float Float deriving (Show, Eq)

sphere :: Int -> (Sphere, Int)
sphere newId = (Sphere newId, newId + 1)

intersect :: Sphere -> Ray -> Intersection
intersect sphere (Ray origin direction) =
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
                in Hit (min t1 t2) (max t1 t2)
    in result   
