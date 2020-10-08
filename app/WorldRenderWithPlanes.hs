module WorldRenderWithPlanes
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Shapes (createSphere, createPlane, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, rotationX, rotationZ, viewTransform, (|<>|), combine)
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let wallMaterial = defaultMaterial { getColor = Color 0.9 0.9 1.0, getSpecular = 0 }
        
        floorMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0 }
        floor' = setMaterial createPlane floorMaterial

        wallDistance = 7
        diagonalWallDistanceX = wallDistance * sqrt 3 / 2
        diagonalWallDistanceZ = wallDistance / 2

        wall1Transform = rotationX (pi / 2) |<>| translation 0 0 wallDistance
        wall1 = flip setMaterial wallMaterial . flip setTransform wall1Transform $ createPlane

        wall2Transform = combine [translation diagonalWallDistanceX 0 diagonalWallDistanceZ, rotationX (pi / 2), rotationZ (-pi / 3)]
        wall2 = flip setMaterial wallMaterial . flip setTransform wall2Transform $ createPlane
        
        wall3Transform = combine [translation diagonalWallDistanceX 0 (-diagonalWallDistanceZ), rotationX (pi / 2), rotationZ (-2 * pi / 3)]
        wall3 = flip setMaterial wallMaterial . flip setTransform wall3Transform $ createPlane
                
        wall4Transform = combine [translation 0 0 (-wallDistance), rotationX (-pi / 2)]
        wall4 = flip setMaterial wallMaterial . flip setTransform wall4Transform $ createPlane

        wall5Transform = combine [translation (-diagonalWallDistanceX) 0 (-diagonalWallDistanceZ), rotationX (-pi / 2), rotationZ (pi / 3)]
        wall5 = flip setMaterial wallMaterial . flip setTransform wall5Transform $ createPlane

        wall6Transform = combine [translation (-diagonalWallDistanceX) 0 diagonalWallDistanceZ, rotationX (-pi / 2), rotationZ (2 * pi / 3)]
        wall6 = flip setMaterial wallMaterial . flip setTransform wall6Transform $ createPlane
                       
        middleSphereTransform = translation (-0.2) 1 0.5
        middleSphereMaterial = defaultMaterial
                                { getColor = Color 0.7 0 0
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        middleSphere = flip setMaterial middleSphereMaterial . flip setTransform middleSphereTransform $ createSphere
                                
        rightSphereTransform = scaling 0.5 0.5 0.5 |<>| translation 1.5 0.5 (-0.5)
        rightSphereMaterial = defaultMaterial
                                { getColor = Color 0.5 1 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        rightSphere = flip setMaterial rightSphereMaterial . flip setTransform rightSphereTransform $ createSphere

        leftSphereTransform = scaling 0.33 0.33 0.33 |<>| translation (-1.5) 0.33 (-0.75)
        leftSphereMaterial = defaultMaterial
                                { getColor = Color 1 0.8 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                }
        leftSphere = flip setMaterial leftSphereMaterial . flip setTransform leftSphereTransform $ createSphere

        light = PointLight (Point 6 6 0) (Color 1 1 1)
        objects = [floor', wall1, wall2, wall3, wall4, wall5, wall6, leftSphere, middleSphere, rightSphere]
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 5 9 (-5)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "world-render-with-planes.ppm" (canvasToPpm image)
    