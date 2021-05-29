module Spec where
import PdePreludat
import Library
import Test.Hspec

auto1 = Auto "AT001LN" [] 1 1 (1,1,1)
auto2 = Auto "DJV214" [] 1 1 (1,1,1)
auto3 = Auto "DJV215" [] 1 1 (1,1,1)
auto4 = Auto "DFH029" [] 1 1 (1,1,1)


correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de costo de Reparacion" $ do
    it "El costo de reperacion de auto1 con patente AT001LN es de 12500" $ do
      costoReparacion auto1 `shouldBe` 12500
    it "El costo de reperacion de auto2 con patente DJV214 es de 18000" $ do
      costoReparacion auto2 `shouldBe` 18000
    it "El costo de reperacion de auto3 con patente DJV215 es de 20000" $ do
      costoReparacion auto3 `shouldBe` 20000
    it "El costo de reperacion de auto4 con patente DFH029 es de 15000" $ do
      costoReparacion auto4 `shouldBe` 15000

