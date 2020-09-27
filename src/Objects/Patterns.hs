{-# LANGUAGE TypeApplications #-}

module Objects.Patterns
    ( Fill (..)
    , Pattern
    , PatternRules (..)
    , createBlendedPattern
    , createCheckerPattern
    , createCombinedCheckerPattern
    , createChecker3dPattern
    , createCombinedChecker3dPattern
    , createGradientPattern
    , createRingPattern
    , createCombinedRingPattern
    , createStripePattern
    , createCombinedStripePattern
    , getPatternInverseTransform
    , getPatternRules
    , getFillColorAt
    , getPatternColorAt
    , setPatternTransform
    ) where

import Common (average)
import Data.Fixed (mod')
import Drawing (Color (..), addColor, multiplyByScalar, subtractColor)
import Matrix (inverse)
import Transform (Transform, identity, transformPoint)
import Space (Point (..))

data PatternRules =
      BlendedRules [Fill] 
    | CheckerRules Fill Fill
    | Checker3dRules Fill Fill
    | GradientRules Color Color
    | RingRules Fill Fill
    | StripeRules Fill Fill
    deriving (Eq, Show)

data Fill = PatternFill Pattern | SolidFill Color
    deriving (Eq, Show)

data Pattern = Pattern
    { getRules :: PatternRules
    , getTransform :: Transform
    , getInverseTransform :: Transform
    } deriving (Eq, Show)

createBlendedPattern :: [Fill] -> Pattern
createBlendedPattern fs = Pattern (BlendedRules fs) identity identity

createCheckerPattern :: Color -> Color -> Pattern
createCheckerPattern firstColor secondColor =
    Pattern (CheckerRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createCombinedCheckerPattern :: Fill -> Fill -> Pattern
createCombinedCheckerPattern firstFill secondFill =
    Pattern (CheckerRules firstFill secondFill) identity identity

createChecker3dPattern :: Color -> Color -> Pattern
createChecker3dPattern firstColor secondColor =
    Pattern (Checker3dRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createCombinedChecker3dPattern :: Fill -> Fill -> Pattern
createCombinedChecker3dPattern firstFill secondFill =
    Pattern (Checker3dRules firstFill secondFill) identity identity

createGradientPattern :: Color -> Color -> Pattern
createGradientPattern firstColor secondColor =
    Pattern (GradientRules firstColor secondColor) identity identity

createRingPattern :: Color -> Color -> Pattern
createRingPattern firstColor secondColor =
    Pattern (RingRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createCombinedRingPattern ::  Fill -> Fill -> Pattern
createCombinedRingPattern firstFill secondFill =
    Pattern (RingRules firstFill secondFill) identity identity

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor =
    Pattern (StripeRules (SolidFill firstColor) (SolidFill secondColor)) identity identity
    
createCombinedStripePattern :: Fill -> Fill -> Pattern
createCombinedStripePattern firstFill secondFill =
    Pattern (StripeRules firstFill secondFill) identity identity

getPatternInverseTransform :: Pattern -> Transform
getPatternInverseTransform = getInverseTransform

getPatternRules :: Pattern -> PatternRules
getPatternRules = getRules

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (Pattern rules _ _) transform
    = Pattern rules transform (inverse transform)

getFillColorAt :: Fill -> Point -> Color

getFillColorAt (SolidFill color) _ = color

getFillColorAt (PatternFill patt) point =
    let patternPoint = transformPoint point (getInverseTransform patt)
    in getPatternColorAt patt patternPoint

getPatternColorAt :: Pattern -> Point -> Color

getPatternColorAt (Pattern (BlendedRules []) _ _) _ =
    Color 0 0 0
getPatternColorAt (Pattern (BlendedRules fills) _ _) point =
    let colors = map (`getFillColorAt` point) fills
    in Color (average $ map getRed colors) (average $ map getGreen colors) (average $ map getBlue colors)

getPatternColorAt (Pattern (CheckerRules firstFill secondFill) _ _) point
    | ((floor . getPointX $ point) + (floor . getPointZ $ point) :: Int) `mod'` 2 == 0 =
        firstFill `getFillColorAt` point
    | otherwise = secondFill `getFillColorAt` point

getPatternColorAt (Pattern (Checker3dRules firstFill secondFill) _ _) point
    | ((floor . getPointX $ point) + (floor . getPointY $ point) + (floor . getPointZ $ point) :: Int) `mod'` 2 == 0 =
        firstFill `getFillColorAt` point
    | otherwise = secondFill `getFillColorAt` point

getPatternColorAt (Pattern (GradientRules firstColor secondColor) _ _) (Point x _ _) =
    let shift = secondColor `subtractColor` firstColor
        fraction = snd @Int . properFraction $ x
    in firstColor `addColor` (shift `multiplyByScalar` fraction)

getPatternColorAt (Pattern (RingRules firstFill secondFill) _ _) point
    | sqrt (getPointX point ** 2 + getPointY point ** 2) `mod'` 2 < 1 = firstFill `getFillColorAt` point
    | otherwise = secondFill `getFillColorAt` point

getPatternColorAt (Pattern (StripeRules firstFill secondFill) _ _) point
    | getPointX point `mod'` 2 < 1 = firstFill `getFillColorAt` point
    | otherwise = secondFill `getFillColorAt` point
