module WorldRenderWithPatterns
    ( renderWorld
    ) where

import Camera (Camera (..), createCamera, render)
import Drawing (Color (..))
import Drawing.Output (canvasToPpm)
import Lights (PointLight (..))
import Objects.Materials (Material (..), defaultMaterial)
import Objects.Patterns
    ( Fill (..)
    , createCombinedCheckerPattern
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , setPatternTransform
    )
import Objects.Shapes (createSphere, createPlane, setMaterial, setTransform)
import Space (Point (..), Vector (..))
import Transform (scaling, translation, rotationX, rotationY, rotationZ, viewTransform, (|<>|))
import World (World (..))

renderWorld :: IO ()
renderWorld = do
    let floorSubPattern = setPatternTransform (createGradientPattern (Color 1 1 1) (Color 0.3 0.3 0.3)) (rotationY (-pi / 2) |<>| scaling 30 30 30)
        floorPattern = createCombinedCheckerPattern (PatternFill floorSubPattern) (SolidFill $ Color 0.24 0.24 0.55)
        floorMaterial = defaultMaterial { getColor = Color 1 0.9 0.9, getSpecular = 0, getPattern = Just floorPattern }
        (fl, fId) = createPlane 0
        floor' = setMaterial fl floorMaterial
                       
        (middleSphere, mId) = createSphere fId
        middleSphereTransform = translation (-0.2) 1 0.5
        middleSpherePattern = setPatternTransform (createRingPattern (Color 0.7 0 0) (Color 0.8 0.7 0)) (scaling 0.15 0.15 0.15 |<>| rotationX (pi / 6))
        middleSphereMaterial = defaultMaterial
                                { getColor = Color 0.7 0 0
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just middleSpherePattern
                                }
        middleSphere' = flip setMaterial middleSphereMaterial . flip setTransform middleSphereTransform $ middleSphere
                                
        (rightSphere, rId) = createSphere mId
        rightSphereTransform = scaling 0.7 0.7 0.7 |<>| translation 1.5 0.5 (-0.5)
        rightSpherePattern = setPatternTransform (createStripePattern (Color 0.5 1 0.1) (Color 1 1 1)) (rotationZ (pi / 2) |<>| rotationX (pi / 4))
        rightSphereMaterial = defaultMaterial
                                { getColor = Color 0.5 1 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just rightSpherePattern
                                }
        rightSphere' = flip setMaterial rightSphereMaterial . flip setTransform rightSphereTransform $ rightSphere

        (leftSphere, _) = createSphere rId
        leftSphereTransform = scaling 0.6 0.6 0.6 |<>| translation (-1.5) 0.33 (-0.75)
        leftSpherePattern = setPatternTransform (createGradientPattern (Color 0.1 0.77 0.89) (Color 1 0.8 0.1)) (rotationZ (pi / 2))
        leftSphereMaterial = defaultMaterial
                                { getColor = Color 1 0.8 0.1
                                , getDiffuse = 0.7
                                , getSpecular = 0.3
                                , getPattern = Just leftSpherePattern
                                }
        leftSphere' = flip setMaterial leftSphereMaterial . flip setTransform leftSphereTransform $ leftSphere

        light = PointLight (Point 3 5 (-6)) (Color 1 1 1)
        objects = [floor', leftSphere', middleSphere', rightSphere']
        world = World objects (Just light)
        cameraTransform = viewTransform (Point 0 5 (-4)) (Point 0 1 0) (Vector 0 1 0)
        camera = (createCamera 640 480 (pi / 3)) { getCameraTransform = cameraTransform }
        image = render camera world
    
    writeFile "world-render-with-patterns.ppm" (canvasToPpm image)
    