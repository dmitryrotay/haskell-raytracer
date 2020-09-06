module Sphere
    ( Sphere (..)
    , createSphere
    , setTransform
    , normalAt
    , setMaterial
    ) where

import Materials (Material (..), defaultMaterial)
import Matrix (transpose, inverse)
import Space (Point (..), Vector (..), subtractPoint, normalize)
import Transform (Transform, identity, transformPoint, transformVector)

data Sphere = Sphere
    { getSphereId :: Int
    , getTransform :: Transform
    , getMaterial :: Material
    } deriving (Show, Eq)

createSphere :: Int -> (Sphere, Int)
createSphere newId =
    let newSphere = Sphere newId identity defaultMaterial
    in (newSphere, newId + 1)

setTransform :: Sphere -> Transform -> Sphere
setTransform (Sphere sphereId _ material) transform = Sphere sphereId transform material

normalAt :: Sphere -> Point -> Vector
normalAt sphere point =
    let objectPoint = transformPoint point (inverse (getTransform sphere))
        objectNormal = objectPoint `subtractPoint` Point 0 0 0
        worldNormal = transformVector
                      objectNormal
                      (transpose . inverse . getTransform $ sphere)
    in normalize worldNormal

setMaterial :: Sphere -> Material -> Sphere
setMaterial sphere material = sphere { getMaterial = material }