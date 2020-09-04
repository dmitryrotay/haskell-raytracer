module Intersections
    ( Intersection (..)
    , hit
    ) where

import Data.List

data Intersection a = Intersection { getObject :: a, getDistance :: Float }
    deriving (Show, Eq)

instance (Eq a) => Ord (Intersection a) where
    compare i1 i2 = compare (getDistance i1) (getDistance i2)

hit :: [Intersection a] -> Maybe (Intersection a)
hit xs = 
    let positiveIntersections = filter (\(Intersection _ t) -> t >= 0) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimumBy
                    (\(Intersection _ t1) (Intersection _ t2) -> compare t1 t2)
                    positiveIntersections