module CameraSpec where

import Camera (Camera (..), createCamera, rayForPixel, render)
import Drawing (Color (..), pixelAt)
import Test.Hspec
import Transform (identity, rotationY, translation, viewTransform, (|<>|))
import Ray (Ray (..))
import Space (Point (..), Vector (..))
import World (defaultWorld)

spec :: Spec
spec =
    describe "Camera" $ do
        describe "newCamera" $ do
            it "constructs a camera with specified parameters and identity transform" $
                let hsize = 160
                    vsize = 120
                    fov = pi / 2
                    camera = createCamera hsize vsize fov
                in do
                    getHsize camera `shouldBe` 160
                    getVsize camera `shouldBe` 120
                    getFov camera `shouldBe` pi / 2
                    getTransform camera `shouldBe` identity
            it "computes pixel size for a horizontal canvas" $ 
                let camera = createCamera 200 125 (pi / 2)
                in getPixelSize camera `shouldBe` 0.01
            it "computes pixel size for a vertical canvas" $ 
                let camera = createCamera 125 200 (pi / 2)
                in getPixelSize camera `shouldBe` 0.01
        
        describe "rayForPixel" $ do
            it "constructs a ray throught the center of the canvas" $
                let camera = createCamera 201 101 (pi / 2)
                    ray = rayForPixel camera 100 50
                in do
                    getOrigin ray `shouldBe` Point 0 0 0
                    getDirection ray `shouldBe` Vector 0 0 (-1)
            it "constructs a ray throught a corner of the canvas" $
                let camera = createCamera 201 101 (pi / 2)
                    ray = rayForPixel camera 0 0
                in do
                    getOrigin ray `shouldBe` Point 0 0 0
                    getDirection ray `shouldBe` Vector 0.66519 0.33259 (-0.66851)
            it "constructs a ray when the camera is transformed" $
                let camera = createCamera 201 101 (pi / 2)
                    camera' = camera { getTransform = translation 0 (-2) 5 |<>| rotationY (pi / 4)}
                    ray = rayForPixel camera' 100 50
                in do
                    getOrigin ray `shouldBe` Point 0 2 (-5)
                    getDirection ray `shouldBe` Vector (sqrt 2 / 2) 0 (-sqrt 2 / 2)
            
        describe "render" $
            it "renders a world with a camera" $
                let world = defaultWorld
                    from = Point 0 0 (-5)
                    to = Point 0 0 0
                    up = Vector 0 1 0
                    camera = (createCamera 11 11 (pi / 2)) { getTransform = viewTransform from to up }
                    image = render camera world
                in pixelAt image 5 5 `shouldBe` Color 0.38066 0.47583 0.2855