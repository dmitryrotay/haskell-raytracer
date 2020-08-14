module Space
    ( SpaceTuple (..)
    , floatEq
    , addS
    , subtractS
    , negateS
    , multiplyS
    , divideS
    , magnitude
    , normalize
    , dot
    , cross
    ) where

floatEq :: Float -> Float -> Bool
x `floatEq` y = abs (x - y) < 1e-5

data SpaceTuple = Point { x :: Float, y :: Float, z :: Float } | Vector { x :: Float, y :: Float, z :: Float }
    deriving (Show)

instance Eq SpaceTuple where
    Point x1 y1 z1 == Point x2 y2 z2 = (x1 `floatEq` x2) && (y1 `floatEq` y2) && (z1 `floatEq` z2)
    Vector x1 y1 z1 == Vector x2 y2 z2 = (x1 `floatEq` x2) && (y1 `floatEq` y2) && (z1 `floatEq` z2)

addS :: SpaceTuple -> SpaceTuple -> SpaceTuple
Point x1 y1 z1 `addS` Vector x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)
Vector x1 y1 z1 `addS` Vector x2 y2 z2 = Vector (x1 + x2) (y1 + y2) (z1 + z2)

negateS :: SpaceTuple -> SpaceTuple
negateS (Point x y z) = Point (-x) (-y) (-z)
negateS (Vector x y z) = Vector (-x) (-y) (-z)

subtractS :: SpaceTuple -> SpaceTuple -> SpaceTuple
Point x1 y1 z1 `subtractS` Point x2 y2 z2 = Vector (x1 - x2) (y1 - y2) (z1 - z2)
Point x1 y1 z1 `subtractS` Vector x2 y2 z2 = Point (x1 - x2) (y1 - y2) (z1 - z2)
Vector x1 y1 z1 `subtractS` Vector x2 y2 z2 = Vector (x1 - x2) (y1 - y2) (z1 - z2)

multiplyS :: SpaceTuple -> Float -> SpaceTuple
Vector x y z `multiplyS` s = Vector (x * s) (y * s) (z * s)

divideS :: SpaceTuple -> Float -> SpaceTuple
Vector x y z `divideS` d = Vector (x / d) (y / d) (z / d)

magnitude :: SpaceTuple -> Float
magnitude (Vector x y z) = sqrt (x^2 + y^2 + z^2)

normalize :: SpaceTuple -> SpaceTuple
normalize (Vector x y z) =
    let m = magnitude (Vector x y z)
    in Vector (x / m) (y / m) (z / m)

dot :: SpaceTuple -> SpaceTuple -> Float
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: SpaceTuple -> SpaceTuple -> SpaceTuple
cross (Vector x1 y1 z1) (Vector x2 y2 z2) =
    let x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2
    in Vector x y z