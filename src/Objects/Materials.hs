module Objects.Materials
    ( Material (..)
    , defaultMaterial
    ) where

import Drawing (Color (..))
import Objects.Patterns (Pattern)

data Material = Material
    { getAmbient :: Double
    , getColor :: Color
    , getDiffuse :: Double
    , getPattern :: Maybe Pattern
    , getReflective :: Double
    , getRefractiveIndex :: Double
    , getShininess :: Double
    , getSpecular :: Double
    , getTransparency :: Double
    } deriving (Eq, Show)

defaultMaterial :: Material
defaultMaterial =
    let color = Color 1 1 1
        ambient = 0.1
        diffuse = 0.9
        specular = 0.9
        shininess = 200.0        
        patt = Nothing
        reflective = 0.0
        transparency = 0.0
        refractiveIndex = 1.0
    in Material ambient color diffuse patt reflective refractiveIndex shininess specular transparency
