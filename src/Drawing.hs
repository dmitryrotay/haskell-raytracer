module Drawing
    ( Color (..)
    , addColor
    , subtractColor
    , multiplyByScalar
    , multiplyByColor
    , Canvas (..)
    , canvas
    , setPixel
    , setPixelMap
    , pixelAt
    , getCoords
    ) where

import           Common ((~==))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M (lookup)
import           Data.Maybe

data Color = Color { red :: Float, green :: Float, blue :: Float } deriving Show

instance Eq Color where
    Color r1 g1 b1 == Color r2 g2 b2 = (r1 ~== r2) && (g1 ~== g2) && (b1 ~== b2)

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
    show (Canvas width height _) = "Canvas {width = " ++ show width ++ ", height = " ++ show height ++ "}"

canvas :: Int -> Int -> Canvas
canvas width height =
    let pixels = [Color 0 0 0 | _ <- [1..width * height]]
    in Canvas width height pixels

setPixel :: Int -> Int -> Color -> Canvas -> Canvas
setPixel x y color (Canvas width height pixels) =
    let n = getIndex width x y
        pixels' = replaceNth n color pixels
    in Canvas width height pixels'

setPixelMap :: HashMap (Int, Int) Color -> Canvas -> Canvas
setPixelMap m (Canvas width height pixels) =
    let pixels' = zipWith (curry (\( (x, y), p) -> fromMaybe p (M.lookup (x, y) m))) (map (getCoords width) [0..]) pixels
    in Canvas width height pixels'

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas width height pixels) x y =
    let n = getIndex width x y
    in pixels !! n

getIndex :: Int -> Int -> Int -> Int
getIndex canvasWidth x y =
    canvasWidth * y + x

getCoords :: Int -> Int -> (Int, Int)
getCoords canvasWidth arrayIndex =
    let y = arrayIndex `div` canvasWidth
        x = arrayIndex - canvasWidth * y
    in (x, y)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
    | n == 0 = val:xs
    | otherwise = x : replaceNth (n - 1) val xs
