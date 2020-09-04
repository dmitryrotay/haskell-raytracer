module World
    ( World (..)
    , createWorld
    , defaultWorld
    , intersectWorld
    ) where
import Data.List (sort)
import Drawing (Color (..))
import Intersections (Intersection (..))
import Intersections.Sphere(intersect, intersectionToList)
import Sphere 
    ( Sphere
    , createSphere
    , setMaterial
    , setTransform
    )
import Lights (PointLight (..))
import Materials (Material (..))
import Ray (Ray (..))
import Space (Point (..))
import Transform (scaling)

data World = World
    { getObjects :: [Sphere]
    , getLight :: Maybe PointLight
    } deriving (Show, Eq)

createWorld :: World
createWorld = World [] Nothing

defaultWorld :: World
defaultWorld =
    let (sphere1, nextId) = createSphere 0
        sphere1' = setMaterial sphere1 (Material (Color 0.8 1.0 0.6) 0.1 0.7 0.2 200.0)
        (sphere2, _) = createSphere nextId
        sphere2' = setTransform sphere2 (scaling 0.5 0.5 0.5)
        light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
    in World [sphere1', sphere2'] (Just light)

intersectWorld :: World -> Ray -> [Intersection Sphere]
intersectWorld (World objects _) ray =
    sort $ concat [intersectionToList (object `intersect` ray) | object <- objects]