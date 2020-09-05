module Camera
    ( Camera (..)
    , createCamera
    , rayForPixel
    ) where

import Matrix (inverse)
import Transform (Transform, identity, transformPoint)
import Ray (Ray (..))
import Space (Point (..), normalize, subtractPoint)

data Camera = Camera
    { getHsize :: Int
    , getVsize :: Int
    , getFov :: Float
    , getTransform :: Transform
    , getHalfWidth :: Float
    , getHalfHeight :: Float
    , getPixelSize :: Float
    }

createCamera :: Int -> Int -> Float -> Camera
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
        inverseTransform = inverse $ getTransform camera
        pixel = transformPoint (Point worldX worldY (-1)) inverseTransform
        origin = transformPoint (Point 0 0 0) inverseTransform
        direction = normalize (pixel `subtractPoint` origin)
    in Ray origin direction
