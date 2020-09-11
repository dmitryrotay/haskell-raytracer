module Shapes
    ( Shape (..)
    , Computations (..)
    , Intersection (..)
    , createSphere
    , createPlane    
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
import Data.Function (on)
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
            , getShapeId :: Int
            , getShapeTransform :: Transform
            , getShapeInverseTransform :: Transform
            , getShapeMaterial :: Material
            } deriving (Show, Eq)

data Intersection = Intersection { getShape :: Shape, getDistance :: Double }
    deriving (Show, Eq)

instance Ord Intersection where
    compare = compare `on` getDistance
   
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
    let newShape = Shape shapeType newId identity identity defaultMaterial
    in (newShape, newId + 1)

normalAt :: Shape -> Point -> Vector
normalAt shape point =
    let shapePoint = transformPoint point (getShapeInverseTransform shape)
        shapeNormal = localNormalAt shape shapePoint
        worldNormal = transformVector
                      shapeNormal
                      (transpose . getShapeInverseTransform $ shape)
    in normalize worldNormal

localNormalAt :: Shape -> Point -> Vector
localNormalAt (Shape Sphere _ _ _ _) point = point `subtractPoint` Point 0 0 0
localNormalAt (Shape Plane _ _ _ _) _ = Vector 0 1 0

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
    in Computations shape distance point overPoint eyeVector normalVector' isInside

hit :: [Intersection] -> Maybe Intersection
hit xs = 
    let positiveIntersections = filter ((>=0) . getDistance) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimum positiveIntersections

setTransform :: Shape -> Transform -> Shape
setTransform (Shape shapeType shapeId _ _ m) newTransform =
    Shape shapeType shapeId newTransform (inverse newTransform) m

setMaterial :: Shape -> Material -> Shape
setMaterial (Shape shapeType shapeId t it _) = Shape shapeType shapeId t it
