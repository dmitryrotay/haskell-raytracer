module Objects.Shapes
    ( Shape (..)
    , ShapeType (..)
    , createCube
    , createGlassSphere
    , createSphere
    , createPlane
    , normalAt
    , localNormalAt
    , setMaterial
    , setTransform
    ) where

import Matrix (inverse, transpose)
import Objects.Materials (Material (..), defaultMaterial)
import Space
    ( Point (..)
    , Vector (..)
    , normalize
    , subtractPoint
    )
import Transform (Transform, identity, transformPoint, transformVector)

data ShapeType = Cube | Plane | Sphere deriving (Show, Eq)

data Shape = Shape
            { getShapeType :: ShapeType
            , getShapeId :: Int
            , getShapeTransform :: Transform
            , getShapeInverseTransform :: Transform
            , getShapeMaterial :: Material
            } deriving (Show, Eq)

createCube :: Shape
createCube = createShape Cube

createSphere :: Shape
createSphere = createShape Sphere

createGlassSphere :: Shape
createGlassSphere =
    let sphere = createSphere
        sphere' = sphere { getShapeMaterial = (getShapeMaterial sphere) { getTransparency = 1.0, getRefractiveIndex = 1.5 } }
    in sphere'

createPlane :: Shape
createPlane = createShape Plane

createShape :: ShapeType -> Shape
createShape shapeType =
    let newShape = Shape shapeType 0 identity identity defaultMaterial
    in newShape

normalAt :: Shape -> Point -> Vector
normalAt shape point =
    let shapePoint = transformPoint point (getShapeInverseTransform shape)
        shapeNormal = localNormalAt shape shapePoint
        worldNormal = transformVector
                      shapeNormal
                      (transpose . getShapeInverseTransform $ shape)
    in normalize worldNormal

localNormalAt :: Shape -> Point -> Vector

localNormalAt (Shape Cube _ _ _ _) (Point x y z) =
    normal
    where maxc = maximum [abs x, abs y, abs z]
          normal
            | maxc == abs x = Vector x 0 0
            | maxc == abs y = Vector 0 y 0
            | otherwise = Vector 0 0 z

localNormalAt (Shape Sphere _ _ _ _) point = point `subtractPoint` Point 0 0 0

localNormalAt (Shape Plane _ _ _ _) _ = Vector 0 1 0

setTransform :: Shape -> Transform -> Shape
setTransform (Shape shapeType shapeId _ _ m) newTransform =
    Shape shapeType shapeId newTransform (inverse newTransform) m

setMaterial :: Shape -> Material -> Shape
setMaterial (Shape shapeType shapeId t it _) = Shape shapeType shapeId t it
