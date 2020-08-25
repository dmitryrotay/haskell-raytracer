module ClockFace
    ( drawClockFace
    ) where

import           Data.Either
import qualified Drawing as D
import qualified Drawing.Output as O
import qualified Matrix as M
import qualified Space as S
import qualified Transform as T
import           Transform ((|<<|))

drawClockFace :: IO ()
drawClockFace = do
    let h = 500
        w = 500
        canvas = D.canvas w h
        originTransform = T.translation (fromIntegral w / 2) (fromIntegral h / 2) 0
        start = Right (M.fromPoint $ S.Point 0 (-(fromIntegral h / 2) + 50) 0)
        hours = start : map (\hour -> start |<<| T.rotationZ (hour * 2 * pi / 12)) [1..11]
        correctOriginHours = map (\x -> (x |<<| originTransform) >>= M.toPoint) hours
        points = map (fromRight $ S.Point 0 0 0) correctOriginHours
        white = D.Color 1 1 1
        canvas' = foldl (\c (S.Point x y _) -> D.setPixel (round x) (round y) white c) canvas points
    putStr . O.canvasToPpm $ canvas'