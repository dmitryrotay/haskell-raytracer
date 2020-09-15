module Silhouette
    ( drawSilhouette
    ) where

import Data.HashMap.Strict (fromList)
import Drawing (Color (..), blankCanvas, setPixelMap, getCoords)
import Drawing.Output (canvasToPpm)
import Objects.Intersections (intersect)
import Objects.Shapes (createSphere, setTransform)
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import Transform (scaling, translation, (|<>|))

drawSilhouette :: IO ()
drawSilhouette = do
    let width = 640 :: Int
        height = 480 :: Int
        foreground = Color 1 0 0
        (sphere, _) = createSphere 0
        translateAndScale = scaling 200 200 200 |<>| translation (fromIntegral width / 2) (fromIntegral height / 2) 0
        transformedSphere = setTransform sphere translateAndScale
        isRayHit x y =
            let ray = Ray (Point x y (-300)) (Vector 0 0 1)
                is = transformedSphere `intersect` ray
            in not . null $ is
        sphereHitsPixelMap = fromList [((x, y), foreground) |
                                       pixelIndex <- [0..width * height - 1],
                                       let (x, y) = getCoords width pixelIndex,
                                       isRayHit (fromIntegral x) (fromIntegral y)
                                      ]
        canvas = blankCanvas width height
        canvas' = setPixelMap canvas sphereHitsPixelMap
    putStr . canvasToPpm $ canvas'
