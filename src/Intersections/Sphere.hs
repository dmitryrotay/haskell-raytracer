module Intersections.Sphere
    ( SphereRayIntersection (..)
    , intersect
    , intersectionToList
    ) where
        
import Intersections (Intersection (..))
import Matrix (inverse)
import Ray (Ray (..), transformRay)
import Space 
    ( Point (..)
    , subtractPoint
    , dot)
import Sphere (Sphere (..))

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

intersect :: Sphere -> Ray -> SphereRayIntersection
intersect sphere ray =
    let sphereToRay = origin `subtractPoint` Point 0 0 0
        inverseTransform = inverse . getTransform $ sphere --possible performance hit
        (Ray origin direction) = transformRay ray inverseTransform
        a = direction `dot` direction
        b = 2 * (direction `dot` sphereToRay)
        c = sphereToRay `dot` sphereToRay - 1
        discriminant =  b ** 2 - 4 * a * c
        result
            | discriminant < 0 = Miss
            | otherwise =
                let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                    p1 = Intersection sphere (min t1 t2)
                    p2 = Intersection sphere (max t1 t2)
                in SphereRayIntersection p1 p2
        in result   

intersectionToList :: SphereRayIntersection -> [Intersection Sphere]
intersectionToList Miss = []
intersectionToList (SphereRayIntersection i1 i2) = [i1, i2]