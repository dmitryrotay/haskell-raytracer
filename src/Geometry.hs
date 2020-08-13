module Geometry
    ( Point (..)
    , Vector (..)
    ) where

floatEq :: Float -> Float -> Bool
x `floatEq` y = abs (x - y) < 1e-5

data Point = Point Float Float Float deriving (Show)

instance Eq Point where
    Point x1 y1 z1 == Point x2 y2 z2 = (x1 `floatEq` x2) && (y1 `floatEq` y2) && (z1 `floatEq` z2)

data Vector = Vector Float Float Float deriving (Show)

instance Eq Vector where
    Vector x1 y1 z1 == Vector x2 y2 z2 = (x1 `floatEq` x2) && (y1 `floatEq` y2) && (z1 `floatEq` z2)