module WorldRenderWithPlanes
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Materials (Material (..), defaultMaterial)
import Shapes (createSphere, createPlane, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import System.IO
import Transform (scaling, translation, rotationX, rotationZ, viewTransform, (|<>|), combine)
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let wallMaterial = defaultMaterial { getColor = Color 0.9 0.9 1.0, getSpecular = 0 }
        
        floorMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0 }
        (fl, fId) = createPlane 0
        floor' = setMaterial fl floorMaterial

        wallDistance = 7
        diagonalWallDistanceX = wallDistance * sqrt 3 / 2
        diagonalWallDistanceZ = wallDistance / 2

        (wall1, wall1Id) = createPlane fId
        wall1Transform = rotationX (pi / 2) |<>| translation 0 0 wallDistance
        wall1' = flip setMaterial wallMaterial . flip setTransform wall1Transform $ wall1

        (wall2, wall2Id) = createPlane wall1Id
        wall2Transform = combine [translation diagonalWallDistanceX 0 diagonalWallDistanceZ, rotationX (pi / 2), rotationZ (-pi / 3)]
        wall2' = flip setMaterial wallMaterial . flip setTransform wall2Transform $ wall2

        (wall3, wall3Id) = createPlane wall2Id
        wall3Transform = combine [translation diagonalWallDistanceX 0 (-diagonalWallDistanceZ), rotationX (pi / 2), rotationZ (-2 * pi / 3)]
        wall3' = flip setMaterial wallMaterial . flip setTransform wall3Transform $ wall3
        
        (wall4, wall4Id) = createPlane wall3Id
        wall4Transform = combine [translation 0 0 (-wallDistance), rotationX (-pi / 2)]
        wall4' = flip setMaterial wallMaterial . flip setTransform wall4Transform $ wall4

        (wall5, wall5Id) = createPlane wall4Id
        wall5Transform = combine [translation (-diagonalWallDistanceX) 0 (-diagonalWallDistanceZ), rotationX (-pi / 2), rotationZ (pi / 3)]
        wall5' = flip setMaterial wallMaterial . flip setTransform wall5Transform $ wall5

        (wall6, wall6Id) = createPlane wall5Id
        wall6Transform = combine [translation (-diagonalWallDistanceX) 0 diagonalWallDistanceZ, rotationX (-pi / 2), rotationZ (2 * pi / 3)]
        wall6' = flip setMaterial wallMaterial . flip setTransform wall6Transform $ wall6
                       
        (middleSphere, mId) = createSphere wall6Id
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

        light = PointLight (Point 6 6 0) (Color 1 1 1)
        objects = [floor', wall1', wall2', wall3', wall4', wall5', wall6', leftSphere', middleSphere', rightSphere']
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 5 9 (-5)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    handle <- openFile "world-render-with-planes.ppm" WriteMode
    hPutStr handle (canvasToPpm image)
    hClose handle