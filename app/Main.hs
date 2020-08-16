module Main where

import qualified Data.HashMap.Strict as M
import           Drawing
import           Drawing.Output
import           ProjectileTrajectory

import           Space

main :: IO ()
main = do
    let start = Point 0 1 0
        velocity = normalize (Vector 1 1.8 0) `multiplyVector` 11.25
        pStart = Projectile start velocity
        gravity = Vector 0 (-0.1) 0
        wind = Vector (-0.01) 0 0
        env = Environment gravity wind
        c = canvas 900 550
        ps = takeWhile (\p -> (py . projectilePosition $ p) > 0) $ scanl (flip tick) pStart (repeat env)
        m = M.fromList $ map (\p -> ((round . px . projectilePosition $ p, 550 - (round . py . projectilePosition $ p)), Color 1 0 0)) ps
        c' = setPixelMap m c
    putStr . canvasToPpm $ c'
