module PatternsSpec where

import Data.Fixed
import Drawing (Color (..))
import Patterns (Pattern (..), createStripePattern, getPatternColorAt)
import Space (Point (..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Patterns" $ do
        let black = Color 0 0 0
            white = Color 1 1 1
        
        describe "createStripePattern" $ do
            it "creates a stripe pattern" $
                let patt = createStripePattern white black
                in do
                    getFirstColor patt `shouldBe` white
                    getSecondColor patt `shouldBe` black

        describe "getPatternColorAt" $ do
            let patt = createStripePattern white black
            it "returns constant value if changing Y coordinate" $ property $
                \y -> patt `getPatternColorAt` Point 0 y 0 `shouldBe` white
            it "returns constant value if changing Z coordinate" $ property $
                \z -> patt `getPatternColorAt` Point 0 0 z `shouldBe` white
            it "returns alternating value if changing X coordinate" $ property $
                \x -> patt `getPatternColorAt` Point x 0 0 `shouldBe` if x `mod'` 2 < 1 then white else black
        
