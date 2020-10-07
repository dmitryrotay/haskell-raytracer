{-# LANGUAGE DataKinds          #-}

module Space
    ( Point (..)    
    , Vector (..)
    , addVectorP
    , addVectorV
    , negateP
    , negateV
    , subtractPoint
    , subtractVectorP
    , subtractVectorV
    , multiplyVector
    , divideVector
    , magnitude
    , normalize
    , dot
    , cross
    , reflectVector
    , vectorToMatrix
    , pointToMatrix
    ) where
        
import Common ((~==))
import Matrix (SpaceMatrix, fromTuple4)

class SpaceElement a where
    getX :: a -> Double
    getY :: a -> Double
    getZ :: a -> Double

eq :: (SpaceElement a) => a -> a -> Bool
e1 `eq` e2 = (getX e1 ~== getX e2) && (getY e1 ~== getY e2) && (getZ e1 ~== getZ e2)

data Point = Point { getPointX :: Double, getPointY :: Double, getPointZ :: Double } deriving (Show)

instance SpaceElement Point where
    getX = getPointX
    getY = getPointY
    getZ = getPointZ

instance Eq Point where
    (==) = eq

data Vector = Vector { getVectorX :: Double, getVectorY :: Double, getVectorZ :: Double } deriving (Show)

instance SpaceElement Vector where
    getX = getVectorX
    getY = getVectorY
    getZ = getVectorZ

instance Eq Vector where
    (==) = eq

addVectorP :: Point -> Vector -> Point
Point x1 y1 z1 `addVectorP` Vector x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)

addVectorV :: Vector -> Vector -> Vector
Vector x1 y1 z1 `addVectorV` Vector x2 y2 z2 = Vector (x1 + x2) (y1 + y2) (z1 + z2)

negateP :: Point -> Point
negateP (Point x y z) = Point (-x) (-y) (-z)

negateV :: Vector -> Vector
negateV (Vector x y z) = Vector (-x) (-y) (-z)

subtractPoint :: Point -> Point -> Vector
Point x1 y1 z1 `subtractPoint` Point x2 y2 z2 = Vector (x1 - x2) (y1 - y2) (z1 - z2)

subtractVectorP :: Point -> Vector -> Point
Point x1 y1 z1 `subtractVectorP` Vector x2 y2 z2 = Point (x1 - x2) (y1 - y2) (z1 - z2)

subtractVectorV :: Vector -> Vector -> Vector
Vector x1 y1 z1 `subtractVectorV` Vector x2 y2 z2 = Vector (x1 - x2) (y1 - y2) (z1 - z2)

multiplyVector :: Vector -> Double -> Vector
Vector x y z `multiplyVector` s = Vector (x * s) (y * s) (z * s)

divideVector :: Vector -> Double -> Vector
Vector x y z `divideVector` d = Vector (x / d) (y / d) (z / d)

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt (x ** 2 + y ** 2 + z ** 2)

normalize :: Vector -> Vector
normalize (Vector x y z) =
    let m = magnitude (Vector x y z)
    in Vector (x / m) (y / m) (z / m)

dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vector -> Vector -> Vector
cross (Vector x1 y1 z1) (Vector x2 y2 z2) =
    let x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2
    in Vector x y z

vectorToMatrix :: Vector -> SpaceMatrix 1
vectorToMatrix (Vector x y z) = fromTuple4 (x, y, z, 0)

pointToMatrix :: Point -> SpaceMatrix 1
pointToMatrix (Point x y z) = fromTuple4 (x, y, z, 1)

reflectVector :: Vector -> Vector -> Vector
reflectVector vector normalVector =
    vector `subtractVectorV`
    ( normalVector `multiplyVector`
      2 `multiplyVector`
      (vector `dot` normalVector)
    )
