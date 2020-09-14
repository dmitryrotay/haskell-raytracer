module Patterns
    ( Pattern (..)
    , createStripePattern
    , setPatternTransform
    ) where

import Drawing (Color (..))
import Matrix (inverse)
import Transform (Transform, identity)

data Pattern = StripePattern
    { getFirstColor :: Color
    , getSecondColor :: Color
    , getPatternTransform :: Transform
    , getPatternInverseTransform :: Transform
    } deriving (Eq, Show)

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor = StripePattern firstColor secondColor identity identity

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (StripePattern firstColor secondColor _ _) transform
    = StripePattern firstColor secondColor transform (inverse transform)
