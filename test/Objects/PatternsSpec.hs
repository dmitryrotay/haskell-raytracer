{-# LANGUAGE TypeApplications #-}

module Objects.PatternsSpec where

import Data.Fixed (mod')
import Drawing (Color (..))
import Objects.Patterns
    ( Pattern,
      Fill (..)    
    , PatternRules (..)
    , createBlendedPattern
    , createCheckerPattern
    , createCombinedCheckerPattern
    , createChecker3dPattern
    , createCombinedChecker3dPattern
    , createGradientPattern
    , createStripePattern
    , createCombinedStripePattern
    , createRingPattern
    , createCombinedRingPattern
    , getFillColorAt
    , getPatternColorAt
    , getPatternInverseTransform
    , getPatternRules
    , setPatternTransform
    )
import Space (Point (..))
import Test.Hspec
import Test.QuickCheck
import Transform (scaling, transformPoint)

spec :: Spec
spec = do
    let black = Color 0 0 0
        white = Color 1 1 1
        checker = createCheckerPattern white black
        checker3d = createChecker3dPattern white black
        gradient = createGradientPattern black white
        ring = createRingPattern white black
        stripe = createStripePattern white black
        fills = 
            [ PatternFill checker
            , PatternFill checker3d
            , PatternFill gradient
            , PatternFill ring
            , PatternFill stripe
            , SolidFill white
            , SolidFill black
            ]
        
        testCombinedPatternConstructor :: (Fill -> Fill -> Pattern) -> (Fill -> Fill -> PatternRules) -> Property
        testCombinedPatternConstructor constructor rulesConstructor = property $ do
            i <- choose (0, length fills - 1)
            j <- choose (0, length fills - 1)
            let firstFill = fills !! i
                secondFill = fills !! j
                patt = constructor firstFill secondFill
            return $ getPatternRules patt `shouldBe` rulesConstructor firstFill secondFill


    describe "createBlendedPattern" $ do
        it "creates a pattern with BlendedRules and a list of passed Fills" $ do
            let (firstFill, secondFill, thirdFill) = (SolidFill white, PatternFill checker, PatternFill ring)
            getPatternRules (createBlendedPattern [firstFill, secondFill, thirdFill])
                `shouldBe` BlendedRules [firstFill, secondFill, thirdFill]
    
    describe "createCheckerPattern" $ do
        it "creates a solid color checker pattern" $ do
            getPatternRules checker `shouldBe` CheckerRules (SolidFill white) (SolidFill black)

    describe "createCombinedCheckerPattern" $ do
        it "creates a checker pattern with two passed fills" $
            testCombinedPatternConstructor createCombinedCheckerPattern CheckerRules

    describe "createChecker3dPattern" $ do
        it "creates a solid color 3D checker pattern" $ do
            getPatternRules checker3d `shouldBe` Checker3dRules (SolidFill white) (SolidFill black)

    describe "createCombinedChecker3dPattern" $ do
        it "creates a 3D checker pattern with two passed fills" $
            testCombinedPatternConstructor createCombinedChecker3dPattern Checker3dRules

    describe "createGradientPattern" $ do
        it "creates a gradient pattern" $ do
            getPatternRules gradient `shouldBe` GradientRules black white
    
    describe "createRingPattern" $ do
        it "creates a solid color ring pattern" $ do
            getPatternRules ring `shouldBe` RingRules (SolidFill white) (SolidFill black)

    describe "createCombinedRingPattern" $ do
        it "creates a ring pattern with two passed fills" $
            testCombinedPatternConstructor createCombinedRingPattern RingRules

    describe "createStripePattern" $ do
        it "creates a solid color stripe pattern" $ do
            getPatternRules stripe `shouldBe` StripeRules (SolidFill white) (SolidFill black)

    describe "createCombinedStripePattern" $ do
        it "creates a stripe pattern with two passed fills" $
            testCombinedPatternConstructor createCombinedStripePattern StripeRules
    
    describe "getFillColorAt" $ do
        let whiteFill = SolidFill white
        it "returns same color at any point for SolidFill" $ property $
            \x y z -> whiteFill `getFillColorAt` Point x y z `shouldBe` white
        
        let patternRingFill = PatternFill ring
        it "returns pattern color at any point for PatternFill with non-transformed pattern" $ property $
            \x y -> patternRingFill `getFillColorAt` Point x y 0 `shouldBe` ring `getPatternColorAt` Point x y 0
        
        let transformedRingPattern = setPatternTransform ring (scaling 0.5 0.5 0.5)
            transformedPatternRingFill = PatternFill transformedRingPattern
        it "returns pattern color at any point for PatternFill with transformed pattern by translating point into pattern space" $ property $
            \x y -> do 
                let point = Point x y 0
                    transformedPoint = transformPoint point (getPatternInverseTransform transformedRingPattern)
                transformedPatternRingFill `getFillColorAt` point `shouldBe` ring `getPatternColorAt` transformedPoint

    describe "getPatternColorAt" $ do
        describe "for blended pattern" $ do
            it "returns color with components calculated as averages of fills' colors at the point for two patterns" $
                let blendedTwoColorPattern = createBlendedPattern [SolidFill black, SolidFill white]
                in property $ \x y z -> blendedTwoColorPattern `getPatternColorAt` Point x y z `shouldBe` Color 0.5 0.5 0.5
            
            it "returns color with components calculated as averages of fills' colors at the point for three patterns" $
                let blendedThreeColorPattern = createBlendedPattern [SolidFill black, SolidFill white, SolidFill (Color 0.2 0.2 0.2)]
                in property $ \x y z -> blendedThreeColorPattern `getPatternColorAt` Point x y z `shouldBe` Color 0.4 0.4 0.4

            it "return black color for blended pattern with no composing patterns" $
                let emptyBlendedPattern = createBlendedPattern []
                in property $ \x y z -> emptyBlendedPattern `getPatternColorAt` Point x y z `shouldBe` Color 0 0 0
            
        describe "for solid color checker pattern" $ do
            it "returns alternating values if changing X coordinate" $ property $
                \x -> checker `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
            it "returns constant value if changing Y coordinate" $ property $
                \y -> checker `getPatternColorAt` Point 0 y 0 `shouldBe` white
            it "returns alternating values if changing Y coordinate" $ property $
                \z -> checker `getPatternColorAt` Point 0 0 z `shouldBe` if z `mod'` 2 < 1 then white else black
            it "returns alternating values if changing X and Z coordinates" $ property $
                \x z -> checker `getPatternColorAt` Point x 0 z
                        `shouldBe` if (floor x + floor z :: Int) `mod'` 2 == 0 then white else black
        
        describe "for solid color checker 3D pattern" $ do
            it "returns alternating values if changing X coordinate" $ property $
                \x -> checker3d `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
            it "returns alternating values if changing Y coordinate" $ property $
                \y -> checker3d `getPatternColorAt` Point 0 y 0 `shouldBe` if y `mod'` 2 < 1 then white else black
            it "returns alternating values if changing Z coordinate" $ property $
                \z -> checker3d `getPatternColorAt` Point 0 0 z `shouldBe` if z `mod'` 2 < 1 then white else black
            it "returns alternating values if changing all coordinates" $ property $
                \x y z -> checker3d `getPatternColorAt` Point x y z
                          `shouldBe` if (floor x + floor y + floor z :: Int) `mod'` 2 == 0 then white else black

        describe "for gradient pattern" $ do
            it "returns constant value if changing Z and Y coordinates" $ property $
                \y z -> gradient `getPatternColorAt` Point 0 y z `shouldBe` black
            it "returns color linearly interpolated between the gradient's colors by X coordinate" $ property $
                \x -> let fraction = snd @Int . properFraction $ x
                      in gradient `getPatternColorAt` Point x 0 0 `shouldBe` Color fraction fraction fraction
        
        describe "for solid color ring pattern" $ do
            it "returns constant value if changing Z coordinate" $ property $
                \z -> ring `getPatternColorAt` Point 0 0 z `shouldBe` white
            it "returns alternating values if changing X coordinate" $ property $
                \x -> ring `getPatternColorAt` Point x 0 0 `shouldBe` if sqrt (x ** 2) `mod'` 2 < 1 then white else black
            it "returns alternating values if changing Y coordinate" $ property $
                \y -> ring `getPatternColorAt` Point 0 y 0 `shouldBe` if sqrt (y ** 2) `mod'` 2 < 1 then white else black
            it "returns alternating values if changing X and Y coordinates" $ property $
                \x y -> ring `getPatternColorAt` Point x y 0 `shouldBe` if sqrt (x ** 2 + y ** 2) `mod'` 2 < 1 then white else black

        describe "for solid color stripe pattern" $ do
            it "returns constant value if changing Y and Z coordinates" $ property $
                \y z -> stripe `getPatternColorAt` Point 0 y z `shouldBe` white
            it "returns alternating values if changing X coordinate" $ property $
                \x -> stripe `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
