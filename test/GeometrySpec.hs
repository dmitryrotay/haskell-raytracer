module GeometrySpec where

import Geometry (Intersection (..), hit)
import Geometry.Sphere (sphere)
import Test.Hspec

spec :: Spec
spec = do
    describe "Geometry" $ do
        describe "hit" $ do
            it "returns hit when all intersection have positive t" $
                let (s, _) = sphere 0
                    i1 = Intersection s 1
                    i2 = Intersection s 2
                in hit [i1, i2] `shouldBe` Just i1
            it "returns only intersection with positive t" $
                let (s, _) = sphere 0
                    i1 = Intersection s (-1)
                    i2 = Intersection s 1
                in hit [i2, i1] `shouldBe` Just i2
            it "returns Nothing when all intersections are negative" $
                let (s, _) = sphere 0
                    i1 = Intersection s (-2)
                    i2 = Intersection s (-1)
                in hit [i1, i2] `shouldBe` Nothing
            it "returns lowest nonnegative intersection" $
                let (s, _) = sphere 0
                    i1 = Intersection s 5
                    i2 = Intersection s 7
                    i3 = Intersection s (-3)
                    i4 = Intersection s 2
                in hit [i1, i2, i3, i4] `shouldBe` Just i4