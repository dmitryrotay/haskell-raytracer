module Camera
    ( Camera (..)
    , createCamera
    , rayForPixel
    , render
    ) where

import Data.HashMap.Strict (fromList)
import Drawing (Canvas, blankCanvas, setPixelMap)
import Matrix (inverse)
import Transform (Transform, identity, transformPoint)
import Ray (Ray (..))
import Space (Point (..), normalize, subtractPoint)
import World (World, colorAt)

data Camera = Camera
    { getHsize :: Int
    , getVsize :: Int
    , getFov :: Double
    , getCameraTransform :: Transform
    , getHalfWidth :: Double
    , getHalfHeight :: Double
    , getPixelSize :: Double
    }

createCamera :: Int -> Int -> Double -> Camera
createCamera hsize vsize fov =
    let halfView = tan (fov / 2.0)
        aspect = fromIntegral hsize / fromIntegral vsize
        (halfWidth, halfHeight)
            | aspect >= 1 = (halfView, halfView / aspect)
            | otherwise = (halfView * aspect, halfView)
        pixelSize = halfWidth * 2 / fromIntegral hsize
    in Camera hsize vsize fov identity halfWidth halfHeight pixelSize

rayForPixel :: Camera -> Int -> Int -> Ray
rayForPixel camera px py =
    let offsetX = (fromIntegral px + 0.5) * getPixelSize camera
        offsetY = (fromIntegral py + 0.5) * getPixelSize camera
        worldX = getHalfWidth camera - offsetX
        worldY = getHalfHeight camera - offsetY
        inverseTransform = inverse $ getCameraTransform camera
        pixel = transformPoint (Point worldX worldY (-1)) inverseTransform
        origin = transformPoint (Point 0 0 0) inverseTransform
        direction = normalize (pixel `subtractPoint` origin)
    in Ray origin direction

render :: Camera -> World -> Canvas
render camera world =
    let hSize = getHsize camera
        vSize = getVsize camera
        image = blankCanvas hSize vSize
        pixelList = [((x, y), color)
                      | x <- [0..hSize - 1]
                      , y <- [0..vSize - 1]
                      , let ray = rayForPixel camera x y
                            color = colorAt world ray
                     ]
        pixelMap = fromList pixelList
        image' = setPixelMap image pixelMap
    in image'
