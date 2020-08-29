module Geometry.SphereSpec where

import Control.Monad

import Geometry (Intersection (..))
import Geometry.Sphere
    ( Sphere (..)
    , SphereRayIntersection (..)
    , sphere
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
                    (s, _) = sphere 0
                    t1 = Intersection s 4.0
                    t2 = Intersection s 6.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray at a tangent" $
                let intersection = raySphereIntersection 0 1 (-5) 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s 5.0
                    t2 = Intersection s 5.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "returns no intersections when ray misses the sphere" $
                let intersection = raySphereIntersection 0 2 (-5) 0 0 1
                in intersection `shouldBe` Miss
            it "computes intersection of sphere by ray originating inside the sphere" $
                let intersection = raySphereIntersection 0 0 0 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s (-1.0)
                    t2 = Intersection s 1.0
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "computes intersection of sphere by ray originating after the sphere" $
                let intersection = raySphereIntersection 0 0 5 0 0 1
                    (s, _) = sphere 0
                    t1 = Intersection s (-6.0)
                    t2 = Intersection s (-4.0)
                in intersection `shouldBe` SphereRayIntersection t1 t2
            it "transforms ray before computing intersection" $
                let r = Ray (Point 0 0 (-5)) (Vector 0 0 1)
                    (s, _) = sphere 0
                    s' = setTransform s (scaling 2 2 2)
                    t1 = Intersection s' 3.0
                    t2 = Intersection s' 7.0
                in intersect s' r `shouldBe` Right (SphereRayIntersection t1 t2)
        describe "sphere" $ do
            it "constructs sphere with identity transformation" $
                let (s, _) = sphere 0
                in getTransform s `shouldBe` identity
        describe "setTransform" $ do
            it "returns sphere object with passed transformation" $
                let (s, _) = sphere 0
                    t = translation 2 3 4
                    s' = setTransform s t
                in getTransform s' `shouldBe` t

raySphereIntersection :: Float -> Float -> Float -> Float -> Float -> Float -> SphereRayIntersection
raySphereIntersection originX originY originZ directionX directionY directionZ =
    let
        origin = Point originX originY originZ
        direction = Vector directionX directionY directionZ
        r = Ray origin direction
        (s, _) = sphere 0
    in case s `intersect` r of
        Left _ -> Miss
        Right intersection -> intersection
