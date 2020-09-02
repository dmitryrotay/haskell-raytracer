module Geometry
    ( Intersection (..)
    , hit
    ) where

import Data.List

data Intersection a = Intersection { getObject :: a, getDistance :: Float }
    deriving (Show, Eq)

hit :: [Intersection a] -> Maybe (Intersection a)
hit xs = 
    let positiveIntersections = filter (\(Intersection _ t) -> t >= 0) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimumBy
                    (\(Intersection _ t1) (Intersection _ t2) -> compare t1 t2)
                    positiveIntersections