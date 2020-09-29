module Objects.Shapes
    ( Shape (..)
    , ShapeType (..)
    , createSphere
    , createPlane
    , normalAt
    , localNormalAt
    , setMaterial
    , setTransform
    ) where

import Matrix (inverse, transpose)
import Objects.Materials (Material, defaultMaterial)
import Space
    ( Point (..)
    , Vector (..)
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

setTransform :: Shape -> Transform -> Shape
setTransform (Shape shapeType shapeId _ _ m) newTransform =
    Shape shapeType shapeId newTransform (inverse newTransform) m

setMaterial :: Shape -> Material -> Shape
setMaterial (Shape shapeType shapeId t it _) = Shape shapeType shapeId t it
