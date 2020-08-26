module ProjectileTrajectory
    ( drawProjectile
    ) where

import Data.HashMap.Strict (fromList)
import Drawing (Color (..), canvas, setPixelMap)
import Drawing.Output (canvasToPpm)
import Space (Point (..), Vector (..), addVectorP, addVectorV, normalize, multiplyVector)

data Projectile = Projectile { projectilePosition :: Point,  projectileVelocity :: Vector }
data Environment = Environment Vector Vector

tick :: Environment -> Projectile -> Projectile
tick (Environment gravity wind) (Projectile position velocity) =
    let position' = position `addVectorP` velocity
        velocity' = velocity `addVectorV` gravity `addVectorV` wind
    in Projectile position' velocity'

drawProjectile :: IO ()
drawProjectile = do
    let start = Point 0 1 0
        velocity = normalize (Vector 1 1.1 0) `multiplyVector` 10
        pStart = Projectile start velocity
        gravity = Vector 0 (-0.1) 0
        wind = Vector (-0.01) 0 0
        env = Environment gravity wind
        c = canvas 900 300
        ps = takeWhile (\p -> (pointY . projectilePosition $ p) > 0) $ scanl (flip tick) pStart (repeat env)
        red = Color 1 0 0
        m = fromList $ map (\p -> ((round . pointX . projectilePosition $ p, 300 - (round . pointY . projectilePosition $ p)), red)) ps
        c' = setPixelMap m c
    putStr . canvasToPpm $ c'
