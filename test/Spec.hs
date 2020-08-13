module Main where

import Test.Hspec

import qualified GeometrySpec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Geometry"     GeometrySpec