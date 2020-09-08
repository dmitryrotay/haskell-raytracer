module Intersections.Sphere
    ( SphereRayIntersection (..)
    , Computations (..)
    , intersect
    , intersectionToList
    , prepareComputations
    ) where

import Common (epsilon)
import Intersections (Intersection (..))
import Matrix (inverse)
import Ray (Ray (..), transformRay, position)
import Space 
    ( Point (..)
    , Vector (..)
    , subtractPoint
    , dot
    , negateV
    , addVectorP
    , multiplyVector
    )
import Sphere (Sphere (..), normalAt)

data SphereRayIntersection = Miss | SphereRayIntersection (Intersection Sphere) (Intersection Sphere)
    deriving (Show, Eq)

data Computations = Computations
    { getCompObject :: Sphere        
    , getCompDistance :: Double
    , getCompPoint :: Point
    , getCompOverPoint :: Point
    , getCompEyeVector :: Vector
    , getCompNormalVector :: Vector
    , getIsInside :: Bool
    } deriving (Show, Eq)

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

prepareComputations :: Intersection Sphere -> Ray -> Computations
prepareComputations (Intersection sphere distance) ray =
    let point = position ray distance
        normalVector = normalAt sphere point
        eyeVector = negateV (getDirection ray)
        isInside = dot normalVector eyeVector < 0
        normalVector'
            | isInside = negateV normalVector
            | otherwise = normalVector
        overPoint = point `addVectorP` (normalVector' `multiplyVector` epsilon)
    in Computations sphere distance point overPoint eyeVector normalVector' isInside
