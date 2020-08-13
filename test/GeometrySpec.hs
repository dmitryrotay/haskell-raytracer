module GeometrySpec where

import Test.Hspec
import Geometry

main :: IO ()
main = hspec $ do
    describe "Geometry" $ do
        describe "Point" $ do
            it "compares with float equality" $ do
                Point 2.2^2 2.2^2 2.2^2 == Point 4.84 4.84 4.84
