module Patterns
    ( Pattern (..)
    , PatternRules (..)
    , createChecker3dPattern
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , setPatternTransform
    ) where

import Drawing (Color (..))
import Matrix (inverse)
import Transform (Transform, identity)

data PatternRules =
      Checker3dRules
        { getRingFirstColor :: Color
        , getRingSecondColor :: Color
        }
    | GradientRules
        { getGradientFirstColor :: Color
        , getGradientSecondColor :: Color
        }
    | RingRules
        { getRingFirstColor :: Color
        , getRingSecondColor :: Color
        }
    | StripeRules
        { getStripeFirstColor :: Color
        , getStripeSecondColor :: Color
        }
    deriving (Eq, Show)

data Pattern = Pattern
    { getPatternRules :: PatternRules
    , getPatternTransform :: Transform
    , getPatternInverseTransform :: Transform
    } deriving (Eq, Show)

createChecker3dPattern :: Color -> Color -> Pattern
createChecker3dPattern firstColor secondColor = Pattern (Checker3dRules firstColor secondColor) identity identity

createGradientPattern :: Color -> Color -> Pattern
createGradientPattern firstColor secondColor = Pattern (GradientRules firstColor secondColor) identity identity

createRingPattern :: Color -> Color -> Pattern
createRingPattern firstColor secondColor = Pattern (RingRules firstColor secondColor) identity identity

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor = Pattern (StripeRules firstColor secondColor) identity identity

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (Pattern rules _ _) transform
    = Pattern rules transform (inverse transform)
