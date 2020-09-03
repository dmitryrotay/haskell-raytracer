module Lights
    ( PointLight (..)
    ) where

import Drawing (Color (..))
import Space (Point (..))

data PointLight = PointLight { getPosition :: Point, getIntensity :: Color }