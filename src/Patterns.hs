module Patterns
    ( Pattern (..)
    , PatternRules (..)
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , setPatternTransform
    ) where

import Drawing (Color (..))
import Matrix (inverse)
import Transform (Transform, identity)

data PatternRules =
      StripeRules
        { getStripeFirstColor :: Color
        , getStripeSecondColor :: Color
        }
    | GradientRules
        { getGradientFirstColor :: Color
        , getGradientSecondColor :: Color
        }
    | RingRules
        { getRingFirstColor :: Color
        , getRingSecondColor :: Color
        }
    deriving (Eq, Show)

data Pattern = Pattern
    { getPatternRules :: PatternRules
    , getPatternTransform :: Transform
    , getPatternInverseTransform :: Transform
    } deriving (Eq, Show)

createGradientPattern :: Color -> Color -> Pattern
createGradientPattern firstColor secondColor = Pattern (GradientRules firstColor secondColor) identity identity

createRingPattern :: Color -> Color -> Pattern
createRingPattern firstColor secondColor = Pattern (RingRules firstColor secondColor) identity identity

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor = Pattern (StripeRules firstColor secondColor) identity identity

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (Pattern rules _ _) transform
    = Pattern rules transform (inverse transform)
