module CameraWorldRender
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Materials (Material (..), defaultMaterial)
import Shapes (createSphere, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, rotationX, rotationY, viewTransform, combine, (|<>|))
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let wallMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0 }
        
        floorTransform = scaling 10 0.01 10
        (fl, fId) = createSphere 0
        floor' = flip setMaterial wallMaterial . flip setTransform floorTransform $ fl
        
        leftWallTransform = combine 
                            [ translation 0 0 5
                            , rotationY (-pi / 4)
                            , rotationX (pi / 2)
                            , scaling 10 0.01 10
                            ]
        (leftWall, lwId) = createSphere fId
        leftWall' = flip setMaterial wallMaterial . flip setTransform leftWallTransform $ leftWall
        
        rightWallTransform = combine 
                            [ translation 0 0 5
                            , rotationY (pi / 4)
                            , rotationX (pi / 2)
                            , scaling 10 0.01 10 
                            ]
        (rightWall, rwId) = createSphere lwId
        rightWall' = flip setMaterial wallMaterial . flip setTransform rightWallTransform $ rightWall
        
        middleSphereTransform = translation (-0.2) 1 0.5
        middleSphereMaterial = defaultMaterial
                                { getColor = Color 0.7 0 0
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        (middleSphere, mId) = createSphere rwId

        middleSphere' = flip setMaterial middleSphereMaterial . flip setTransform middleSphereTransform $ middleSphere
                                
        rightSphereTransform = scaling 0.5 0.5 0.5 |<>| translation 1.5 0.5 (-0.5)
        rightSphereMaterial = defaultMaterial
                                { getColor = Color 0.5 1 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        (rightSphere, rId) = createSphere mId
        rightSphere' = flip setMaterial rightSphereMaterial . flip setTransform rightSphereTransform $ rightSphere

        leftSphereTransform = scaling 0.33 0.33 0.33 |<>| translation (-1.5) 0.33 (-0.75)
        leftSphereMaterial = defaultMaterial
                                { getColor = Color 1 0.8 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        (leftSphere, _) = createSphere rId
        leftSphere' = flip setMaterial leftSphereMaterial . flip setTransform leftSphereTransform $ leftSphere

        light = PointLight (Point 10 10 (-10)) (Color 1 1 1)
        objects = [floor', leftWall', rightWall', leftSphere', middleSphere', rightSphere']
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 0 1.5 (-5)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "camera-world-render.ppm" (canvasToPpm image)
