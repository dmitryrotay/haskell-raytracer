module Materials
    ( Material (..)
    , defaultMaterial
    , lighting
    ) where

import Drawing (Color (..), addColor, multiplyByColor, multiplyByScalar)
import Lights (PointLight (..))
import Patterns (Pattern, getPatternColorAt)
import Space (Point, Vector, subtractPoint, dot, reflectVector, negateV, normalize)

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

lighting :: Material -> PointLight -> Point -> Vector -> Vector -> Bool -> Color
lighting material light position eyeVector normalVector inShadow =
    let color = case getPattern material of
            Nothing -> getColor material
            Just patt -> getPatternColorAt patt position
        effectiveColor = color `multiplyByColor` getIntensity light
        ambient = effectiveColor `multiplyByScalar` getAmbient material
        
        resultColor
            | inShadow = ambient
            | otherwise =
                let lightVector = normalize (getPosition light `subtractPoint` position)
                    lightDotNormal = lightVector `dot` normalVector
                    black = Color 0 0 0    
                    (diffuse, specular)
                        | lightDotNormal < 0 = (black, black)
                        | otherwise = 
                            let diff = effectiveColor
                                    `multiplyByScalar` getDiffuse material
                                    `multiplyByScalar` lightDotNormal
                                spec =
                                    let reflectionVector = reflectVector (negateV lightVector) normalVector
                                        reflectionDotEye = reflectionVector `dot` eyeVector
                                        result
                                            | reflectionDotEye <= 0 = black
                                            | otherwise =
                                                let factor = reflectionDotEye ** getShininess material
                                                in getIntensity light
                                                    `multiplyByScalar` getSpecular material
                                                    `multiplyByScalar` factor
                                    in result
                            in (diff, spec)
                    in ambient `addColor` diffuse `addColor` specular

    in resultColor
