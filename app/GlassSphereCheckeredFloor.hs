module GlassSphereCheckeredFloor
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns (createCheckerPattern)
import Objects.Shapes (Shape (..), createPlane, createSphere, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, viewTransform, (|<>|))
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let floorPattern = createCheckerPattern (Color 0.85 0.85 0.85) (Color 0.15 0.15 0.15)
        floor' = setTransform (createPlane { getShapeMaterial = defaultMaterial { getAmbient = 0.8, getDiffuse = 0.2, getPattern = Just floorPattern } }) (scaling 0.7 0.7 0.7)
                       
        sphere1 = setTransform 
                      createSphere { getShapeMaterial = defaultMaterial { getColor = Color 0.2 0.2 0.2
                                                                   , getAmbient = 0.0
                                                                   , getDiffuse = 0.0
                                                                   , getSpecular = 0.9
                                                                   , getShininess = 300.0
                                                                   , getTransparency = 0.9
                                                                   , getRefractiveIndex = 1.5
                                                                   , getReflective = 0.9
                                                                   }}
                      (translation 0 4.0 0)

        sphere2 = setTransform
                      createSphere { getShapeMaterial = defaultMaterial { getColor = Color 0.2 0.2 0.2
                                                                   , getAmbient = 0.0
                                                                   , getDiffuse = 0.0
                                                                   , getSpecular = 0.9
                                                                   , getShininess = 300.0
                                                                   , getTransparency = 0.9
                                                                   , getRefractiveIndex = 1.0000034
                                                                   , getReflective = 0.9
                                                                   }}
                      (scaling 0.5 0.5 0.5 |<>| translation 0 4.0 0)

        light = PointLight (Point (-8) 10 (-8)) (Color 1 1 1)
        objects = [floor', sphere1, sphere2]
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 0 8 0) (Point 0 0 0) (Vector (-1) 0 0)
        camera = (createCamera 300 300 0.6) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "glass-sphere-checkered-floor.ppm" (canvasToPpm image)
    