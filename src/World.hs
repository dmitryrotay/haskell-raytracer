module World
    ( World (..)
    , createWorld
    , defaultWorld
    , intersectWorld
    , setLight
    , shadeHit
    , colorAt
    , isShadowed
    ) where
import Data.List (sort)
import Drawing (Color (..))
import Lights (PointLight (..))
import Materials (Material (..), lighting)
import Ray (Ray (..))
import Shapes
    ( Shape (..)
    , Intersection (..)
    , Computations (..)
    , createSphere
    , hit
    , intersect
    , prepareComputations
    , setMaterial
    , setTransform
    )
import Space (Point (..), subtractPoint, magnitude, normalize)
import Transform (scaling)

data World = World
    { getShapes :: [Shape]
    , getLight :: Maybe PointLight
    } deriving (Show, Eq)

createWorld :: World
createWorld = World [] Nothing

defaultWorld :: World
defaultWorld =
    let (sphere1, nextId) = createSphere 0
        sphere1' = setMaterial sphere1 (Material (Color 0.8 1.0 0.6) 0.1 0.7 0.2 200.0)
        (sphere2, _) = createSphere nextId
        sphere2' = setTransform sphere2  (scaling 0.5 0.5 0.5)
        light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
    in World [sphere1', sphere2'] (Just light)

intersectWorld :: World -> Ray -> [Intersection]
intersectWorld (World shapes _) ray =
    sort $ concat [shape `intersect` ray | shape <- shapes]

setLight :: World -> PointLight -> World
setLight (World shapes _) light = World shapes (Just light)

colorAt :: World -> Ray -> Color
colorAt (World shapes (Just light)) ray =
    let world = World shapes (Just light)
        xs = intersectWorld world ray
        objectHit = hit xs
        color = case objectHit of
                  Nothing -> Color 0 0 0
                  Just intersection ->
                    let comps = prepareComputations intersection ray
                    in shadeHit world comps
    in color
colorAt _ _ = Color 0 0 0

shadeHit :: World -> Computations -> Color
shadeHit world comps =
    case world of
        (World _ (Just light)) ->
            let shadowed = isShadowed world (getCompOverPoint comps)
            in lighting
                    (getShapeMaterial $ getCompShape comps)
                    light
                    (getCompOverPoint comps)
                    (getCompEyeVector comps)
                    (getCompNormalVector comps)
                    shadowed
        _ -> Color 0 0 0

isShadowed :: World -> Point -> Bool
isShadowed world point =
    case world of
        (World _ Nothing) -> True
        (World _ (Just light)) ->
            let vector = getPosition light `subtractPoint` point
                distance = magnitude vector
                direction = normalize vector
                ray = Ray point direction
                intersections = intersectWorld world ray
                h = hit intersections
            in case h of
                Nothing -> False
                Just (Intersection _ t) -> t < distance
