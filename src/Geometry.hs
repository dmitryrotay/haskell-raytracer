module Geometry
    ( Intersection (..)
    , hit
    ) where

data Intersection a = Intersection a Float
    deriving (Show, Eq)

instance Eq a => Ord (Intersection a) where
    compare (Intersection _ t1) (Intersection _ t2) = compare t1 t2

hit :: Eq a => [Intersection a] -> Maybe (Intersection a)
hit xs = 
    let positiveIntersections = filter (\(Intersection _ t) -> t >= 0) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimum positiveIntersections