module ClockFace
    ( drawClockFace
    ) where

import           Data.Either
import qualified Drawing as D
import qualified Drawing.Output as O
import qualified Matrix as M
import qualified Space as S
import qualified Transform as T
import           Transform ((|<>|))

drawClockFace :: IO ()
drawClockFace = do
    let h = 320
        w = 480
        canvas = D.canvas w h
        originTransform = T.translation (fromIntegral w / 2) (fromIntegral h / 2) 0
        start = Right (M.fromPoint $ S.Point 0 (-(fromIntegral h / 2) + 50) 0)
        hours = start : map (\hour -> start >>= (|<>| T.rotationZ (hour * 2 * pi / 12))) [1..11]
        correctOriginHours = map (\x -> (x >>= (|<>| originTransform)) >>= M.toPoint) hours
        points = map (fromRight $ S.Point 0 0 0) correctOriginHours
        red = D.Color 1 0 0
        canvas' = foldl (\c (S.Point x y _) -> D.setPixel (round x) (round y) red c) canvas points
    putStr . O.canvasToPpm $ canvas'
