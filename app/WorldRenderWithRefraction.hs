module WorldRenderWithRefraction
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns
    ( createCheckerPattern
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , setPatternTransform
    )
import Objects.Shapes (createSphere, createPlane, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, rotationX, rotationZ, viewTransform, (|<>|))
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let grey = Color 0.85 0.85 0.85
        chocolate = Color 0.48 0.25 0
        checker = createCheckerPattern grey chocolate

        floorMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0, getPattern = Just checker }
        floor' = setMaterial createPlane floorMaterial

        wallMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0, getPattern = Just checker }
        wallTransform = rotationX (- pi / 2) |<>| translation 0 0 10
        wall = flip setMaterial wallMaterial . flip setTransform wallTransform $ createPlane
                       
        middleSphereTransform = translation (-0.2) 1 0.5
        middleSpherePattern = setPatternTransform (createRingPattern (Color 0.7 0 0) (Color 0.8 0.7 0)) (scaling 0.15 0.15 0.15 |<>| rotationX (pi / 6))
        middleSphereMaterial = defaultMaterial
                                { getColor = Color 0.7 0 0
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just middleSpherePattern
                                }
        middleSphere = flip setMaterial middleSphereMaterial . flip setTransform middleSphereTransform $ createSphere
                                
        rightSphereTransform = scaling 0.7 0.7 0.7 |<>| translation 1.5 0.5 (-0.5)
        rightSpherePattern = setPatternTransform (createStripePattern (Color 0.5 1 0.1) (Color 1 1 1)) (rotationZ (pi / 2) |<>| rotationX (pi / 6))
        rightSphereMaterial = defaultMaterial
                                { getColor = Color 0.5 1 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just rightSpherePattern
                                }
        rightSphere = flip setMaterial rightSphereMaterial . flip setTransform rightSphereTransform $ createSphere

        leftSphereTransform = scaling 0.6 0.6 0.6 |<>| translation (-1.5) 0.33 (-0.75)
        leftSpherePattern = setPatternTransform (createGradientPattern (Color 0.1 0.77 0.89) (Color 1 0.8 0.1)) (rotationZ (pi / 2))
        leftSphereMaterial = defaultMaterial
                                { getColor = Color 1 0.8 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just leftSpherePattern
                                }
        leftSphere = flip setMaterial leftSphereMaterial . flip setTransform leftSphereTransform $ createSphere

        glassMaterial = defaultMaterial { getColor = Color 0.2 0.2 0.2
                                        , getAmbient = 0.0
                                        , getDiffuse = 0.0
                                        , getSpecular = 0.9
                                        , getShininess = 300.0
                                        , getTransparency = 0.9
                                        , getRefractiveIndex = 1.5
                                        , getReflective = 0.9
                                        }
        glassSphereTransform = scaling 1.2 1.2 1.2 |<>| translation (-1.8) 4.5 (-4)
        glassSphere = flip setMaterial glassMaterial . flip setTransform glassSphereTransform $ createSphere

        light = PointLight (Point 3 5 (-6)) (Color 1 1 1)
        objects = [floor', wall, leftSphere, middleSphere, rightSphere, glassSphere]
        world = World objects (Just light)
        cameraTransform = viewTransform (Point (-2.7) 6.5 (-6.5)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "world-render-with-reflections.ppm" (canvasToPpm image)
    