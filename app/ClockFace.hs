module ClockFace
    ( drawClockFace
    ) where

import Drawing (Color (..), blank, setPixel)
import Drawing.Output (canvasToPpm)
import Space (Point (..), transformPoint)
import Transform (translation, rotationZ)

drawClockFace :: IO ()
drawClockFace = do
    let width = 640
        height = 480
        canvas = blank width height
        originTransform = translation (fromIntegral width / 2) (fromIntegral height / 2) 0
        start = Point 0 (-(fromIntegral height / 2) + 50) 0
        hours = start : map (\hour -> start `transformPoint` rotationZ (hour * 2 * pi / 12)) [1..11]
        correctOriginHours = map (`transformPoint` originTransform) hours
        red = Color 1 0 0
        canvas' = foldl
            (\c (Point x y _) -> setPixel c (round x) (round y) red)
            canvas
            correctOriginHours
    putStr . canvasToPpm $ canvas'
