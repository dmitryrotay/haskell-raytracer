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

data Color = Color { red :: Float, green :: Float, blue :: Float } deriving (Show)

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
    show (Canvas a b _) = "Canvas {width = " ++ show a ++ ", height = " ++ show b ++ "}"

canvas :: Int -> Int -> Canvas
canvas w h =
    let pixels = [Color 0 0 0 | _ <- [1..w * h]]
    in Canvas w h pixels

setPixel :: Int -> Int -> Color -> Canvas -> Canvas
setPixel x y c (Canvas w h ps) =
    let n = getIndex w x y
        ps' = replaceNth n c ps
    in Canvas w h ps'

setPixelMap :: HashMap (Int, Int) Color -> Canvas -> Canvas
setPixelMap m (Canvas w h ps) =
    let ps' = zipWith (curry (\( (x, y), p) -> fromMaybe p (M.lookup (x, y) m))) (map (getCoords w) [0..]) ps
    in Canvas w h ps'

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas w h ps) x y =
    let n = getIndex w x y
    in ps !! n

getIndex :: Int -> Int -> Int -> Int
getIndex w x y =
    w * y + x

getCoords :: Int -> Int -> (Int, Int)
getCoords w i =
    let y = i `div` w
        x = i - w * y
    in (x, y)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
    | n == 0 = val:xs
    | otherwise = x : replaceNth (n - 1) val xs
