module Objects.Intersections
    ( Computations (..) 
    , Intersection (..)
    , hit
    , intersect
    , localIntersect
    , prepareComputations
    ) where

import Common (epsilon)
import Data.Function (on)
import Objects.Shapes (Shape (..), ShapeType (..), normalAt)
import Ray (Ray (..), position, transformRay)
import Space
    ( Point (..)
    , Vector (..)
    , addVectorP
    , dot
    , multiplyVector
    , negateV
    , subtractPoint
    , reflectVector
    )

data Computations = Computations
    { getCompShape :: Shape
    , getCompDistance :: Double
    , getCompPoint :: Point
    , getCompOverPoint :: Point
    , getCompEyeVector :: Vector
    , getCompNormalVector :: Vector
    , getReflectionVector :: Vector
    , getIsInside :: Bool
    } deriving (Show, Eq)

data Intersection = Intersection { getShape :: Shape, getDistance :: Double }
    deriving (Show, Eq)

instance Ord Intersection where
    compare = compare `on` getDistance

hit :: [Intersection] -> Maybe Intersection
hit xs = 
    let positiveIntersections = filter ((>=0) . getDistance) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimum positiveIntersections

intersect :: Shape -> Ray -> [Intersection]
intersect shape ray =
    let localRay = transformRay ray $ getShapeInverseTransform shape
    in localIntersect shape localRay

localIntersect :: Shape -> Ray -> [Intersection]

localIntersect (Shape Sphere sphereId t it m) (Ray origin direction) =
    let sphere = Shape Sphere sphereId t it m
        sphereToRay = origin `subtractPoint` Point 0 0 0
        a = direction `dot` direction
        b = 2 * (direction `dot` sphereToRay)
        c = sphereToRay `dot` sphereToRay - 1
        discriminant =  b ** 2 - 4 * a * c
        result
            | discriminant < 0 = []
            | otherwise =
                let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                    p1 = Intersection sphere (min t1 t2)
                    p2 = Intersection sphere (max t1 t2)
                in [p1, p2]
    in result

localIntersect (Shape Plane sphereId t it m) (Ray origin direction) =
    intersection
    where
        plane = Shape Plane sphereId t it m
        directionY = getVectorY direction
        intersection
            | abs directionY < epsilon = []
            | otherwise =
                let distance = -getPointY origin / directionY
                in [Intersection plane distance]

prepareComputations :: Intersection -> Ray -> Computations
prepareComputations (Intersection shape distance) ray =
    let point = position ray distance
        normalVector = normalAt shape point
        eyeVector = negateV (getDirection ray)
        isInside = dot normalVector eyeVector < 0
        normalVector'
            | isInside = negateV normalVector
            | otherwise = normalVector
        overPoint = addVectorP point . multiplyVector normalVector' $ epsilon
        reflectedVector = reflectVector (getDirection ray) normalVector'
    in Computations shape distance point overPoint eyeVector normalVector' reflectedVector isInside
