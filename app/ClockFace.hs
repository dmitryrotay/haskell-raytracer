module ClockFace
    ( drawClockFace
    ) where

import Data.Either
import Drawing (Color (..), canvas, setPixel)
import Drawing.Output (canvasToPpm)
import Space (Point (..), Vector (..), transformPoint)
import Transform (translation, rotationZ, (|<>|))

drawClockFace :: IO ()
drawClockFace = do
    let h = 480
        w = 640
        outputCanvas = canvas w h
        originTransform = translation (fromIntegral w / 2) (fromIntegral h / 2) 0
        start = Point 0 (-(fromIntegral h / 2) + 50) 0
        hours = start : map (\hour -> start `transformPoint` rotationZ (hour * 2 * pi / 12)) [1..11]
        correctOriginHours = map (`transformPoint` originTransform) hours
        red = Color 1 0 0
        outputCanvas' = foldl
            (\c (Point x y _) -> setPixel (round x) (round y) red c)
            outputCanvas
            correctOriginHours
    putStr . canvasToPpm $ outputCanvas'
