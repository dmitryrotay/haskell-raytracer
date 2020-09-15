module Objects
    ( computeObjectPerceivedColor
    , getPatternColorForObjectAt
    ) where

import Drawing (Color (..), addColor, multiplyByColor, multiplyByScalar)
import Lights (PointLight (..))
import Objects.Materials (Material (..))
import Objects.Patterns (Pattern (..), getPatternColorAt)
import Objects.Shapes (Shape (..))
import Space
    ( Point (..)
    , Vector (..)
    , negateV
    , dot
    , normalize
    , subtractPoint
    , reflectVector
    )
import Transform (transformPoint)
  
computeObjectPerceivedColor :: Material -> Shape -> PointLight -> Point -> Vector -> Vector -> Bool -> Color
computeObjectPerceivedColor material object light point eyeVector normalVector inShadow =
    let color = case getPattern material of
            Nothing -> getColor material
            Just patt -> getPatternColorForObjectAt patt object point
        effectiveColor = color `multiplyByColor` getIntensity light
        ambient = effectiveColor `multiplyByScalar` getAmbient material
        
        resultColor
            | inShadow = ambient
            | otherwise =
                let lightVector = normalize (getPosition light `subtractPoint` point)
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

getPatternColorForObjectAt :: Pattern -> Shape -> Point -> Color
getPatternColorForObjectAt patt object point =
    let objectPoint = transformPoint point (getShapeInverseTransform object)
        patternPoint = transformPoint objectPoint (getPatternInverseTransform patt)
    in getPatternColorAt patt patternPoint
