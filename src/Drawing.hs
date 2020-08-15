module Drawing
    ( Color (..)
    , addColor
    , subtractColor
    , multiplyByScalar
    , multiplyByColor
    , Canvas (..)
    , canvas
    , setPixel
    , pixelAt
    ) where

import Common

data Color = Color { red :: Float, green :: Float, blue :: Float } deriving (Show)

instance Eq Color where
    Color r1 g1 b1 == Color r2 g2 b2 = (r1 `floatEq` r2) && (g1 `floatEq` g2) && (b1 `floatEq` b2)

addColor :: Color -> Color -> Color
Color r1 g1 b1 `addColor` Color r2 g2 b2 = Color (r1 + r2) (g1 + g2) (b1 + b2)

subtractColor :: Color -> Color -> Color
Color r1 g1 b1 `subtractColor` Color r2 g2 b2 = Color (r1 - r2) (g1 - g2) (b1 - b2)

multiplyByScalar :: Color -> Float -> Color
Color r g b `multiplyByScalar` x = Color (r * x) (g * x) (b * x)

multiplyByColor :: Color -> Color -> Color
Color r1 g1 b1 `multiplyByColor` Color r2 g2 b2 = Color (r1 * r2) (g1 * g2) (b1 * b2)

data Canvas = Canvas { width :: Int, height :: Int, pixels :: [Color] }

instance Show Canvas where 
    show (Canvas a b _) = "Canvas {width = " ++ show a ++ ", height = " ++ show b ++ "}"

canvas :: Int -> Int -> Canvas
canvas w h =
    let pixels = [Color 0 0 0 | _ <- [1..w * h]]
    in Canvas w h pixels

setPixel :: Canvas -> Int -> Int -> Color -> Canvas
setPixel (Canvas w h ps) x y c =
    let n = getIndex w h x y
        ps' = replaceNth n c ps
    in Canvas w h ps'

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas w h ps) x y =
    let n = getIndex w h x y
    in ps !! n

getIndex :: Int -> Int -> Int -> Int -> Int
getIndex w h x y =
    w * y + x + 1

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
    | n == 0 = val:xs
    | otherwise = x : replaceNth (n - 1) val xs