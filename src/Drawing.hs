module Drawing
    ( Color (..)
    , addColor
    , subtractColor
    , multiplyByScalar
    , multiplyByColor
    , Canvas (..)
    , blank
    , setPixel
    , setPixelMap
    , pixelAt
    , getCoords
    ) where

import           Common ((~==))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M (lookup)
import           Data.Maybe

data Color = Color { getRed :: Float, getGreen :: Float, getBlue :: Float } deriving Show

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

data Canvas = Canvas { getWidth :: Int, getHeight :: Int, getPixels :: [Color] }

instance Show Canvas where 
    show (Canvas width height _) = "Canvas {width = " ++ show width ++ ", height = " ++ show height ++ "}"

blank :: Int -> Int -> Canvas
blank width height =
    let pixels = [Color 0 0 0 | _ <- [1..width * height]]
    in Canvas width height pixels

setPixel :: Canvas -> Int -> Int -> Color -> Canvas
setPixel (Canvas width height pixels) x y color =
    let n = getIndex width x y
        pixels' = replaceNth n color pixels
    in Canvas width height pixels'

setPixelMap :: Canvas -> HashMap (Int, Int) Color -> Canvas
setPixelMap (Canvas width height pixels) m =
    let pixels' = zipWith (curry (\( (x, y), p) -> fromMaybe p (M.lookup (x, y) m)))
                                                   (map (getCoords width) [0..]) pixels
    in Canvas width height pixels'

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas width _ pixels) x y =
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
