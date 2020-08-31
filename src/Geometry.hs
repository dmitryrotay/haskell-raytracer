{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures     #-}

module Geometry
    ( Intersection (..)
    , hit
    ) where

import Data.List

data Intersection :: * -> * where
    Intersection :: (Eq a, Show a) =>
        a
        -> Float
        -> Intersection a 

deriving instance Show (Intersection a)
deriving instance Eq (Intersection a)

instance Ord (Intersection a) where
    compare (Intersection _ t1) (Intersection _ t2) = compare t1 t2

hit :: (Eq a) => [Intersection a] -> Maybe (Intersection a)
hit xs = 
    let positiveIntersections = filter (\(Intersection _ t) -> t >= 0) xs
    in case positiveIntersections of
        [] -> Nothing
        _ -> Just $ minimum positiveIntersections
