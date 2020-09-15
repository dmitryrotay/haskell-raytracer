{-# LANGUAGE TypeApplications #-}

module Objects.PatternsSpec where

import Data.Fixed (mod')
import Drawing (Color (..))
import Objects.Patterns
    ( Pattern (..)
    , PatternRules (..)
    , createChecker3dPattern
    , createGradientPattern
    , createStripePattern
    , createRingPattern
    , getPatternColorAt
    )
import Test.Hspec
import Test.QuickCheck
import Space (Point (..))

spec :: Spec
spec = do
    describe "Objects.Patterns" $ do
        let black = Color 0 0 0
            white = Color 1 1 1
            checker3d = createChecker3dPattern white black
            gradient = createGradientPattern black white
            ring = createRingPattern white black
            stripe = createStripePattern white black
        
        describe "createChecker3dPattern" $ do
            it "creates a 3D checker pattern" $ do
                case checker3d of
                    (Pattern (Checker3dRules firstColor secondColor) _ _) ->
                        do firstColor `shouldBe` white
                           secondColor `shouldBe` black
                    _ -> expectationFailure "Not a Checker3d pattern"
        
        describe "createGradientPattern" $ do
            it "creates a gradient pattern" $ do
                case gradient of
                    (Pattern (GradientRules firstColor secondColor) _ _) ->
                        do firstColor `shouldBe` black
                           secondColor `shouldBe` white
                    _ -> expectationFailure "Not a Gradient pattern"
        
        describe "createRingPattern" $ do
            it "creates a ring pattern" $ do
                case ring of
                    (Pattern (RingRules firstColor secondColor) _ _) ->
                        do firstColor `shouldBe` white
                           secondColor `shouldBe` black
                    _ -> expectationFailure "Not a Ring pattern"

        describe "createStripePattern" $ do
            it "creates a stripe pattern" $ do
                case stripe of
                    (Pattern (StripeRules firstColor secondColor) _ _) ->
                        do firstColor `shouldBe` white
                           secondColor `shouldBe` black
                    _ -> expectationFailure "Not a Stripe pattern"
        
        describe "getPatternColorAt" $ do
            describe "for checker 3D pattern" $ do
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
            
            describe "for ring pattern" $ do
                it "returns constant value if changing Z coordinate" $ property $
                    \z -> ring `getPatternColorAt` Point 0 0 z `shouldBe` white
                it "returns alternating values if changing X coordinate" $ property $
                    \x -> ring `getPatternColorAt` Point x 0 0 `shouldBe` if sqrt (x ** 2) `mod'` 2 < 1 then white else black
                it "returns alternating values if changing Y coordinate" $ property $
                    \y -> ring `getPatternColorAt` Point 0 y 0 `shouldBe` if sqrt (y ** 2) `mod'` 2 < 1 then white else black
                it "returns alternating values if changing X and Y coordinates" $ property $
                    \x y -> ring `getPatternColorAt` Point x y 0 `shouldBe` if sqrt (x ** 2 + y ** 2) `mod'` 2 < 1 then white else black

            describe "for stripe pattern" $ do
                it "returns constant value if changing Y and Z coordinates" $ property $
                    \y z -> stripe `getPatternColorAt` Point 0 y z `shouldBe` white
                it "returns alternating values if changing X coordinate" $ property $
                    \x -> stripe `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
