module SphereWithLighting
    ( drawSphere
    ) where

import Data.HashMap.Strict (fromList)
import Drawing (Color (..), blankCanvas, setPixelMap, getCoords)
import Drawing.Output (canvasToPpm)
import Intersections (Intersection (..), hit)
import Intersections.Sphere (SphereRayIntersection (..), intersect)
import Lights (PointLight (..))
import Materials (Material (..), lighting)
import Ray (Ray (..), position)
import Space (Point (..), negateV, subtractPoint, normalize)
import Sphere (Sphere (..), createSphere, setMaterial, normalAt)

width :: Int
width = 640

height :: Int
height = 480

canvasWidth :: Float
canvasWidth = 4.8

canvasHeight :: Float
canvasHeight = 3.6

canvasOffset :: Float
canvasOffset = 2

eyeOffset :: Float
eyeOffset = 5

toObject :: (Int, Int) -> (Float, Float)
toObject (worldX, worldY) =
    let objectX = canvasWidth / fromIntegral width * fromIntegral worldX - (canvasWidth / 2)
        objectY = canvasHeight / fromIntegral height * fromIntegral worldY - (canvasHeight / 2)
    in (objectX, objectY)

drawSphere :: IO ()
drawSphere = do
    let (blankSphere, _) = createSphere 0
        sphereColor = Color 0.7 0 0
        material = Material sphereColor 0.2 0.9 0.9 200.0
        sphere = setMaterial blankSphere material
        light = PointLight (Point 10 10 (-10)) (Color 1 1 1)
        eyePosition = Point 0 0 (-eyeOffset)
        background = Color 0 0 0
        
        castRay worldX worldY = 
            let (x, y) = toObject (worldX, worldY)
                rayDirection = normalize $ Point x y canvasOffset `subtractPoint` eyePosition
                ray = Ray eyePosition rayDirection
                intersection = sphere `intersect` ray
                isRayHit = intersection /= Miss
                color = case intersection of
                        Miss -> background
                        SphereRayIntersection i1 i2 ->
                            let rayHit = hit [i1, i2]
                            in case rayHit of
                                Nothing -> background
                                Just (Intersection s t) -> 
                                    let point = position ray t
                                        normal = normalAt s point
                                        eye = negateV (getDirection ray)
                                    in lighting (getMaterial s) light point eye normal
            in (isRayHit, color)
                   
        sphereHitsPixelMap = fromList [((x, y), color) |
                                       pixelIndex <- [0..width * height - 1]
                                       , let (x, y) = getCoords width pixelIndex
                                             (isRayHit, color) = castRay x y
                                       , isRayHit
                                      ]
        canvas = blankCanvas width height
        canvas' = setPixelMap canvas sphereHitsPixelMap
    putStr . canvasToPpm $ canvas'