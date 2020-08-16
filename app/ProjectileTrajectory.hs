module ProjectileTrajectory
    ( Projectile (..)
    , Environment (..)
    , tick
    ) where

import Space

data Projectile = Projectile { projectilePosition :: Point,  projectileVelocity :: Vector }
data Environment = Environment Vector Vector

tick :: Environment -> Projectile -> Projectile
tick (Environment gravity wind) (Projectile position velocity) =
    let position' = position `addVectorP` velocity
        velocity' = velocity `addVectorV` gravity `addVectorV` wind
    in Projectile position' velocity'
