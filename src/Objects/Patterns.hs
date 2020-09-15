{-# LANGUAGE TypeApplications #-}

module Objects.Patterns
    ( Pattern (..)
    , PatternRules (..)
    , createChecker3dPattern
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , getPatternColorAt
    , setPatternTransform
    ) where

import Data.Fixed (mod')
import Drawing (Color (..), addColor, multiplyByScalar, subtractColor)
import Matrix (inverse)
import Transform (Transform, identity)
import Space (Point (..))

data PatternRules =
      Checker3dRules
        { getFirstColor :: Color
        , getSecondColor :: Color
        }
    | GradientRules
        { getFirstColor :: Color
        , getSecondColor :: Color
        }
    | RingRules
        { getFirstColor :: Color
        , getSecondColor :: Color
        }
    | StripeRules
        { getFirstColor :: Color
        , getSecondColor :: Color
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

getPatternColorAt :: Pattern -> Point -> Color
getPatternColorAt (Pattern (Checker3dRules firstColor secondColor) _ _) (Point x y z)
    | (floor x + floor y + floor z :: Int) `mod'` 2 == 0 = firstColor
    | otherwise = secondColor
getPatternColorAt (Pattern (GradientRules firstColor secondColor) _ _) (Point x _ _) =
    let shift = secondColor `subtractColor` firstColor
        fraction = snd @Int . properFraction $ x
    in firstColor `addColor` (shift `multiplyByScalar` fraction)
getPatternColorAt (Pattern (RingRules firstColor secondColor) _ _) (Point x y _)
    | sqrt (x ** 2 + y ** 2) `mod'` 2 < 1 = firstColor
    | otherwise = secondColor
getPatternColorAt (Pattern (StripeRules firstColor secondColor) _ _) (Point x _ _)
    | x `mod'` 2 < 1 = firstColor
    | otherwise = secondColor
