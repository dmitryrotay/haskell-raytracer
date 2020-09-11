module Shapes
    ( Shape (..)
    , Computations (..)
    , Intersection (..)
    , createSphere
    , createPlane
    , getMaterial
    , getTransform
    , intersect
    , localIntersect
    , normalAt
    , localNormalAt
    , prepareComputations
    , hit
    , setMaterial
    , setTransform
    ) where

import Common (epsilon)
import Data.List (minimumBy)
import Materials (Material, defaultMaterial)
import Matrix (inverse, transpose)
import Ray (Ray (..), position, transformRay)
import Space
    ( Point (..)
    , Vector (..)
    , multiplyVector
    , addVectorP
    , negateV
    , dot
    , normalize
    , subtractPoint
    )
import Transform (Transform, identity, transformPoint, transformVector)

data ShapeType = Sphere | Plane deriving (Show, Eq)

data Shape = Shape
            { getShapeType :: ShapeType
            , getSphereId :: Int
            , getSphereTransform :: Transform
            , getSphereMaterial :: Material
            } deriving (Show, Eq)

data Intersection = Intersection { getShape :: Shape, getDistance :: Double }
    deriving (Show, Eq)

instance Ord Intersection where
    compare i1 i2 = compare (getDistance i1) (getDistance i2)
   
data Computations = Computations
    { getCompShape :: Shape
    , getCompDistance :: Double
    , getCompPoint :: Point
    , getCompOverPoint :: Point
    , getCompEyeVector :: Vector
    , getCompNormalVector :: Vector
    , getIsInside :: Bool
    } deriving (Show, Eq)

createSphere :: Int -> (Shape, Int)
createSphere = createShape Sphere

createPlane :: Int -> (Shape, Int)
createPlane = createShape Plane

createShape :: ShapeType -> Int -> (Shape, Int)
createShape shapeType newId =
    let newShape = Shape shapeType newId identity defaultMaterial
    in (newShape, newId + 1)

normalAt :: Shape -> Point -> Vector
normalAt shape point =
    let shapePoint = transformPoint point (inverse (getTransform shape))
        shapeNormal = localNormalAt shape shapePoint
        worldNormal = transformVector
                      shapeNormal
                      (transpose . inverse . getTransform $ shape)
    in normalize worldNormal

localNormalAt :: Shape -> Point -> Vector
localNormalAt (Shape Sphere _ _ _) point = point `subtractPoint` Point 0 0 0
localNormalAt (Shape Plane _ _ _) _ = Vector 0 1 0

intersect :: Shape -> Ray -> [Intersection]
intersect shape ray =
    let inverseTransform = inverse . getTransform $ shape
        shapeRay = transformRay ray inverseTransform
    in localIntersect shape shapeRay

localIntersect :: Shape -> Ray -> [Intersection]

localIntersect (Shape Sphere sphereId t m) (Ray origin direction) =
    let sphere = Shape Sphere sphereId t m
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

localIntersect (Shape Plane sphereId t m) (Ray origin direction) =
    intersection
    where
        plane = Shape Plane sphereId t m
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
        overPoint = point `addVectorP` (normalVector' `multiplyVector` epsilon)
    in Computations shape distance point overPoint eyeVector normalVector' isInside

hit :: [Intersection] -> Maybe Intersection
hit xs = 
    let positiveIntersections = filter (\(Intersection _ t) -> t >= 0) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimumBy
                    (\(Intersection _ t1) (Intersection _ t2) -> compare t1 t2)
                    positiveIntersections

getTransform :: Shape -> Transform
getTransform (Shape _ _ shapeTransform _) = shapeTransform

setTransform :: Shape -> Transform -> Shape
setTransform (Shape shapeType shapeId _ m) newTransform = Shape shapeType shapeId newTransform m

getMaterial :: Shape -> Material
getMaterial (Shape _ _ _ shapeMaterial) = shapeMaterial

setMaterial :: Shape -> Material -> Shape
setMaterial (Shape shapeType shapeId t _) = Shape shapeType shapeId t