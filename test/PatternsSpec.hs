module PatternsSpec where

import Drawing (Color (..))
import Patterns (Pattern (..), PatternRules (..), createStripePattern)
import Test.Hspec

spec :: Spec
spec = do
    describe "Patterns" $ do
        let black = Color 0 0 0
            white = Color 1 1 1
            patt = createStripePattern white black
        
        describe "createStripePattern" $ do
            it "creates a stripe pattern" $ do
                (getFirstColor . getPatternRules) patt `shouldBe` white
                (getSecondColor . getPatternRules) patt `shouldBe` black  
