module Materials
    ( Material (..)
    , defaultMaterial
    ) where

import Drawing (Color (..))
import Patterns (Pattern)

data Material = Material
    { getColor :: Color
    , getAmbient :: Double
    , getDiffuse :: Double
    , getSpecular :: Double
    , getShininess :: Double
    , getPattern :: Maybe Pattern
    } deriving (Eq, Show)

defaultMaterial :: Material
defaultMaterial =
    let color = Color 1 1 1
        ambient = 0.1
        diffuse = 0.9
        specular = 0.9
        shininess = 200.0
        patt = Nothing
    in Material color ambient diffuse specular shininess patt
