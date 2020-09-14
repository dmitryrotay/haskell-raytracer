module Patterns
    ( Pattern (..)
    , PatternRules (..)
    , createStripePattern
    , setPatternTransform
    ) where

import Drawing (Color (..))
import Matrix (inverse)
import Transform (Transform, identity)

data PatternRules = StripeRules
    { getFirstColor :: Color
    , getSecondColor :: Color
    } deriving (Eq, Show)

data Pattern = Pattern
    { getPatternRules :: PatternRules
    , getPatternTransform :: Transform
    , getPatternInverseTransform :: Transform
    } deriving (Eq, Show)

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor = Pattern (StripeRules firstColor secondColor) identity identity

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (Pattern rules _ _) transform
    = Pattern rules transform (inverse transform)
