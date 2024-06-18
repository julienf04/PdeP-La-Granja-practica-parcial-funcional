module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de si un animal tiene nombre falopa" $ do
    it "Tiene nombre falopa si su nombre termina con una determinada letra" $ do
      tieneNombreFalopa vacaLoca `shouldBe` True

