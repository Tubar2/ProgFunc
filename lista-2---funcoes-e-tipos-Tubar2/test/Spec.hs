module Main where
import Test.Hspec
import StdLogic
import Complex

z1 = Rect (sqrt 0.5) (sqrt 0.5)
z2 = Polar 1 0
z3 = Polar 1 45

main :: IO ()
main = hspec $ do
  stdLogicTests
  complexTests

stdLogicTests = do
  describe "stdLogic-show" $ do
    it "T" $ do
      show T `shouldBe` "T"
    it "F" $ do
      show F `shouldBe` "F"
    it "X" $ do
      show X `shouldBe` "X"
    it "Z" $ do
      show Z `shouldBe` "Z"
  describe "stdLogic-eq" $ do
    it "T" $ do
      T == T `shouldBe` True
    it "F" $ do
      F == F `shouldBe` True
    it "X" $ do
      X == X `shouldBe` True
    it "Z" $ do
      Z == Z `shouldBe` True
    it "T" $ do
      T == F `shouldBe` False
  describe "stdLogic-adder" $ do
    it "Adder test 1: " $ do
      (F,T) `shouldBe` adder F T T
    it "Adder test 2: " $ do
      (T,T) `shouldBe` adder T T T
    it "Adder test 3: " $ do
     (X,T) `shouldBe` adder T T Z
    it "Adder test 4: " $ do
      (X,F) `shouldBe` adder Z F F
    it "Adder test 5: " $ do
      (X,X) `shouldBe` adder X Z T
  describe "stdLogic-mux" $ do
    it "Mux test 1:" $ do
      F `shouldBe` mux T F T
    it "Mux test 2:" $ do
      X `shouldBe` mux T F X
    it "Mux test 3:" $ do
      T `shouldBe` mux T X F
    it "Mux test 4:" $ do
      X `shouldBe` mux T Z T
    it "Mux test 5:" $ do
      F `shouldBe` mux F F F

complexTests =  do
  describe "complex-num" $ do
    it "z1 + z2" $ do
      Rect (1 + sqrt 0.5) (sqrt 0.5) `shouldBe` (z1 + z2)
    it "z1 - z2" $ do
      Rect (sqrt 0.5 - 1) (sqrt 0.5) `shouldBe` (z1 - z2)
    it "z1 * z2" $ do
      Polar 1 90 `shouldBe` (z1 * z3)
  describe "complex-predicates" $ do
    it "isRect z1" $ do
      isRect z1 `shouldBe` True
    it "isRect z2" $ do
      isRect z2 `shouldBe` False
    it "isPolar z1" $ do
      isPolar z1 `shouldBe` False
    it "isPolar z2" $ do
      isPolar z2 `shouldBe` True
  describe "complex-conversion" $ do
    it "toRect z1" $ do
      Rect (sqrt 0.5) (sqrt 0.5) `shouldBe` toRect z1
    it "toRect z2" $ do
      Rect 1 0 `shouldBe` toRect z2
    it "toPolar z1" $ do
      Polar 1 45 `shouldBe` toPolar z1
    it "toPolar z2" $ do
      Polar 1 0 `shouldBe` toPolar z2
  describe "complex-show" $ do
    it "show 1 + 2i" $ do
      show (Rect 1 2) == "1.0 + 2.0i"
    it "show 1 - 2i" $ do
      show (Rect 1 (-2)) == "1.0 - 2.0i"
    it "show 3" $ do
      show (Rect 3 0) == "3.0"
    it "show 1#45" $ do
      show (Polar 1 45) == "1.0#45.0"
    it "show 2#90" $ do
      show (Polar 2 90) == "2.0#90.0"
