module ProjectileTrajectory
    ( drawProjectile
    ) where

import qualified Data.HashMap.Strict as M
import qualified Drawing as D
import qualified Drawing.Output as O
import qualified Space as S

data Projectile = Projectile { projectilePosition :: S.Point,  projectileVelocity :: S.Vector }
data Environment = Environment S.Vector S.Vector

tick :: Environment -> Projectile -> Projectile
tick (Environment gravity wind) (Projectile position velocity) =
    let position' = position `S.addVectorP` velocity
        velocity' = velocity `S.addVectorV` gravity `S.addVectorV` wind
    in Projectile position' velocity'

drawProjectile :: IO ()
drawProjectile = do
    let start = S.Point 0 1 0
        velocity = S.normalize (S.Vector 1 1.1 0) `S.multiplyVector` 10
        pStart = Projectile start velocity
        gravity = S.Vector 0 (-0.1) 0
        wind = S.Vector (-0.01) 0 0
        env = Environment gravity wind
        c = D.canvas 900 300
        ps = takeWhile (\p -> (S.pointY . projectilePosition $ p) > 0) $ scanl (flip tick) pStart (repeat env)
        red = D.Color 1 0 0
        m = M.fromList $ map (\p -> ((round . S.pointX . projectilePosition $ p, 300 - (round . S.pointY . projectilePosition $ p)), red)) ps
        c' = D.setPixelMap m c
    putStr . O.canvasToPpm $ c'
