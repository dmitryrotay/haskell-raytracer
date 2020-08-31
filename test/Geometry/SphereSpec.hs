module Geometry.SphereSpec where

import Control.Monad

import Geometry (Intersection (..))
import Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , createSphere
    , intersect
    , setTransform
    )

import Ray (Ray (..))

import Space (Point (..), Vector (..))
import Test.Hspec
import Transform (identity, translation, scaling)

spec :: Spec
spec = do
    describe "Sphere" $ do
        describe "intersect" $ do
            it "computes intersection of sphere by ray at two points" $
                let intersection = raySphereIntersection 0 0 (-5) 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere 4.0
                    t2 = Intersection sphere 6.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere 5.0
                    t2 = Intersection sphere 5.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere (-1.0)
                    t2 = Intersection sphere 1.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    (sphere, _) = createSphere 0
                    t1 = Intersection sphere (-6.0)
                    t2 = Intersection sphere (-4.0)
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "transforms ray before computing intersection" $
                let r = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (sphere, _) = createSphere 0
                    sphere' = setTransform sphere (scaling 2 2 2)
                    t1 = Intersection sphere' 3.0
                    t2 = Intersection sphere' 7.0
                in intersect sphere' r `shouldBe` Right (SphereRayIntersection t1 t2)
        describe "sphere" $ do
            it "constructs sphere with identity transformation" $
                let (sphere, _) = createSphere 0
                in getTransform sphere `shouldBe` identity
        describe "setTransform" $ do
            it "returns sphere object with passed transformation" $
                let (sphere, _) = createSphere 0
                    t = translation 2 3 4
                    sphere' = setTransform sphere t
                in getTransform sphere' `shouldBe` t

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> SphereRayIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        ray = Ray origin direction
        (sphere, _) = createSphere 0
    in case sphere `intersect` ray of
        Left _ -> Miss
        Right intersection -> intersection
