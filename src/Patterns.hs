module Patterns
    ( Pattern (..)
    , createStripePattern
    , getPatternColorAt
    ) where

import Data.Fixed (mod')
import Drawing (Color (..))
import Space (Point (..))

data Pattern = StripePattern { getFirstColor :: Color, getSecondColor :: Color } deriving (Eq, Show)

createStripePattern :: Color -> Color -> Pattern
createStripePattern = StripePattern

getPatternColorAt :: Pattern -> Point -> Color
getPatternColorAt (StripePattern firstColor secondColor) (Point x _ _)
    | x `mod'` 2 < 1 = firstColor
    | otherwise = secondColor
