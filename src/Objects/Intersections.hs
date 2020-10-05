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
import Data.List (delete)
import Objects.Materials (Material (..))
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
    , subtractVectorP
    , reflectVector
    )

data Computations = Computations
    { getCompShape :: Shape
    , getCompDistance :: Double
    , getCompPoint :: Point
    , getCompOverPoint :: Point
    , getCompUnderPoint :: Point
    , getCompEyeVector :: Vector
    , getCompNormalVector :: Vector
    , getCompReflectionVector :: Vector
    , getIsInside :: Bool
    , getCompN1 :: Double
    , getCompN2 :: Double
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

prepareComputations :: Intersection -> Ray -> [Intersection] -> Computations
prepareComputations intersection ray allIntersections =
    let shape = getShape intersection
        distance = getDistance intersection
        point = position ray distance
        normalVector = normalAt shape point
        eyeVector = negateV (getDirection ray)
        isInside = dot normalVector eyeVector < 0
        normalVector'
            | isInside = negateV normalVector
            | otherwise = normalVector
        normalEpsilonVector = normalVector' `multiplyVector` epsilon
        overPoint = point `addVectorP` normalEpsilonVector
        underPoint = point `subtractVectorP` normalEpsilonVector
        reflectedVector = reflectVector (getDirection ray) normalVector'
        (n1, n2) = computeRefractionParameters intersection allIntersections
    in Computations shape distance point overPoint underPoint eyeVector normalVector' reflectedVector isInside n1 n2

computeRefractionParameters :: Intersection -> [Intersection] -> (Double, Double)
computeRefractionParameters intersection allIntersections =
    let processIntersection :: ([Shape], Double, Double) -> Intersection -> ([Shape], Double, Double)
        processIntersection (containers, currentN1, currentN2) i =
            let n1' = if i == intersection
                      then case containers of
                          [] -> 1.0
                          x:_ -> (getRefractiveIndex . getShapeMaterial) x
                      else currentN1
                shape = getShape i
                containers' = if shape `elem` containers
                              then delete shape containers
                              else shape : containers
                n2' = if i == intersection
                      then case containers' of
                          [] -> 1.0
                          x:_ -> (getRefractiveIndex . getShapeMaterial) x
                      else currentN2
            in (containers', n1', n2')

        (_, n1, n2) = foldl processIntersection ([], 1.0, 1.0) allIntersections
    in (n1, n2)
