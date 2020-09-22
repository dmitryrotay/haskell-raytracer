{-# LANGUAGE TypeApplications #-}

module Objects.Patterns
    ( Fill (..)
    , Pattern (..)
    , PatternRules (..)
    , createCheckerPattern
    , createChecker3dPattern
    , createCombinedRingPattern
    , createGradientPattern
    , createRingPattern
    , createStripePattern
    , getFillColorAt
    , getPatternColorAt
    , setPatternTransform
    ) where

import Data.Fixed (mod')
import Drawing (Color (..), addColor, multiplyByScalar, subtractColor)
import Matrix (inverse)
import Transform (Transform, identity)
import Space (Point (..))

data PatternRules =
      CheckerRules Fill Fill
    | Checker3dRules Fill Fill
    | GradientRules Color Color
    | RingRules Fill Fill
    | StripeRules Fill Fill
    deriving (Eq, Show)

data Fill = PatternFill Pattern | SolidFill Color
    deriving (Eq, Show)

data Pattern = Pattern
    { getPatternRules :: PatternRules
    , getPatternTransform :: Transform
    , getPatternInverseTransform :: Transform
    } deriving (Eq, Show)

createCheckerPattern :: Color -> Color -> Pattern
createCheckerPattern firstColor secondColor =
    Pattern (CheckerRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createChecker3dPattern :: Color -> Color -> Pattern
createChecker3dPattern firstColor secondColor =
    Pattern (Checker3dRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createCombinedRingPattern :: Pattern -> Pattern -> Pattern
createCombinedRingPattern firstPattern secondPattern =
    Pattern (RingRules (PatternFill firstPattern) (PatternFill secondPattern)) identity identity

createGradientPattern :: Color -> Color -> Pattern
createGradientPattern firstColor secondColor =
    Pattern (GradientRules firstColor secondColor) identity identity

createRingPattern :: Color -> Color -> Pattern
createRingPattern firstColor secondColor =
    Pattern (RingRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

createStripePattern :: Color -> Color -> Pattern
createStripePattern firstColor secondColor =
    Pattern (StripeRules (SolidFill firstColor) (SolidFill secondColor)) identity identity

setPatternTransform :: Pattern -> Transform -> Pattern
setPatternTransform (Pattern rules _ _) transform
    = Pattern rules transform (inverse transform)

getFillColorAt :: Fill -> Point -> Color
getFillColorAt (SolidFill color) _ = color
getFillColorAt (PatternFill patt) point =
    getPatternColorAt patt point

getPatternColorAt :: Pattern -> Point -> Color
getPatternColorAt (Pattern (CheckerRules firstFill secondFill) _ _) point
    | ((floor . getPointX $ point) + (floor . getPointZ $ point) :: Int) `mod'` 2 == 0 =
        getFillColorAt firstFill point
    | otherwise = getFillColorAt secondFill point
getPatternColorAt (Pattern (Checker3dRules firstFill secondFill) _ _) point
    | ((floor . getPointX $ point) + (floor . getPointY $ point) + (floor . getPointZ $ point) :: Int) `mod'` 2 == 0 =
        getFillColorAt firstFill point
    | otherwise = getFillColorAt secondFill point
getPatternColorAt (Pattern (GradientRules firstColor secondColor) _ _) (Point x _ _) =
    let shift = secondColor `subtractColor` firstColor
        fraction = snd @Int . properFraction $ x
    in firstColor `addColor` (shift `multiplyByScalar` fraction)
getPatternColorAt (Pattern (RingRules firstFill secondFill) _ _) point
    | sqrt (getPointX point ** 2 + getPointY point ** 2) `mod'` 2 < 1 = getFillColorAt firstFill point
    | otherwise = getFillColorAt secondFill point
getPatternColorAt (Pattern (StripeRules firstFill secondFill) _ _) point
    | getPointX point `mod'` 2 < 1 = getFillColorAt firstFill point
    | otherwise = getFillColorAt secondFill point
