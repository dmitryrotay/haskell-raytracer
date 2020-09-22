module WorldRenderWithPatterns
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns (Pattern (..), createCheckerPattern)
import Objects.Shapes (createSphere, createPlane, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, viewTransform, (|<>|))
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let first = Color 1 1 1
        second = Color 0.6 0.6 0.9
        floorPattern = (createCheckerPattern first second) { getPatternTransform = scaling 0.1 0.1 0.1 }

        floorMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0, getPattern = Just floorPattern }
        (fl, fId) = createPlane 0
        floor' = setMaterial fl floorMaterial
                       
        (middleSphere, mId) = createSphere fId
        middleSphereTransform = translation (-0.2) 1 0.5
        middleSphereMaterial = defaultMaterial
                                { getColor = Color 0.7 0 0
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        middleSphere' = flip setMaterial middleSphereMaterial . flip setTransform middleSphereTransform $ middleSphere
                                
        (rightSphere, rId) = createSphere mId
        rightSphereTransform = scaling 0.5 0.5 0.5 |<>| translation 1.5 0.5 (-0.5)
        rightSphereMaterial = defaultMaterial
                                { getColor = Color 0.5 1 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        rightSphere' = flip setMaterial rightSphereMaterial . flip setTransform rightSphereTransform $ rightSphere

        (leftSphere, _) = createSphere rId
        leftSphereTransform = scaling 0.33 0.33 0.33 |<>| translation (-1.5) 0.33 (-0.75)
        leftSphereMaterial = defaultMaterial
                                { getColor = Color 1 0.8 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        leftSphere' = flip setMaterial leftSphereMaterial . flip setTransform leftSphereTransform $ leftSphere

        light = PointLight (Point 3 5 (-6)) (Color 1 1 1)
        objects = [floor', leftSphere', middleSphere', rightSphere']
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 0 5 (-6)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "world-render-with-patterns.ppm" (canvasToPpm image)
    