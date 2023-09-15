module Main where

import Test.Hspec
import Syntax
import Interpreter
import VM
import Display
import qualified Data.Map as M

main :: IO ()
main = hspec $ do
  testEval
  testEvalStr
  testDisplay
  testCompile
  testRunBytecode
  testEvalV

testEval = do
  describe "eval" $ do
    it "1" $ do
      eval (Num 1) `shouldBe` Just 1
    it "1 + 1" $ do
      eval (Add (Num 1) (Num 1)) `shouldBe` Just 2
    it "3 * 14" $ do
      eval (Mult (Num 3) (Num 14)) `shouldBe` Just 42
    it "17 - 5" $ do
      eval (Sub (Num 17) (Num 5)) `shouldBe` Just 12
    it "1 + (3 * 14) - (17 - 5)" $ do
      eval (Sub (Add (Num 1) (Mult (Num 3) (Num 14))) (Sub (Num 17) (Num 5)))
        `shouldBe` Just 31
    it "1 / 0" $ do
      eval (Div (Num 1) (Num 0)) `shouldBe` Nothing
    it "3 + (1 / 0)" $ do
      eval (Add (Num 3) (Div (Num 1) (Num 0))) `shouldBe` Nothing

testEvalStr = do
  describe "evalStr" $ do
    it "1" $ do
      evalStr "1" `shouldBe` Just 1
    it "2 + 3" $ do
      evalStr "2 + 3" `shouldBe` Just 5
    it "3 +" $ do
      evalStr "3 +" `shouldBe` Nothing
    it "3 / 0" $ do
      evalStr "3 / 0" `shouldBe` Nothing
    it "3 + 5 * 2 + 8" $ do
      evalStr "3 + 5 * 2 + 8" `shouldBe` Just 21

testDisplay = do
  describe "display" $ do
    it "display 1" $ do
      display (Num 1) `shouldBe` "1"
    it "display 2 + 3" $ do
      display (Add (Num 2) (Num 3)) `shouldBe` "(2 + 3)"
    it "display (Add (Num 1) (Add (Num 2) (Num 3)))" $ do
      display (Add (Num 1) (Add (Num 2) (Num 3))) `shouldBe` "(1 + (2 + 3))"
    it "display ((2 + 3) * 5)" $ do
      display (Mult (Add (Num 2) (Num 3)) (Num 5)) `shouldBe` "((2 + 3) * 5)"

testCompile = do
  describe "compile" $ do
    it "compile 1" $ do
      compile (Num 1) `shouldBe` [PUSH 1]
    it "compile 1 + 2" $ do
      compile (Add (Num 1) (Num 2)) `elem`
        [[PUSH 1, PUSH 2, ADD], [PUSH 2, PUSH 1, ADD]] `shouldBe` True
    it "compile 2 - 1" $ do
      compile (Sub (Num 2) (Num 1)) `shouldBe` [PUSH 2, PUSH 1, SUB]
    it "compile 6 / 2" $ do
      compile (Div (Num 6) (Num 2)) `shouldBe` [PUSH 6, PUSH 2, DIV]
    it "compile 1 * 2" $ do
      compile (Mult (Num 1) (Num 2)) `elem`
        [[PUSH 1, PUSH 2, MULT], [PUSH 2, PUSH 1, MULT]] `shouldBe` True
    it "compile 1 * 2 + 3" $ do
      compile (Add (Mult (Num 1) (Num 2)) (Num 3)) `elem`
        [ [PUSH 1, PUSH 2, MULT, PUSH 3, ADD]
        , [PUSH 2, PUSH 1, MULT, PUSH 3, ADD]
        , [PUSH 3, PUSH 2, PUSH 1, MULT, ADD]
        , [PUSH 3, PUSH 1, PUSH 2, MULT, ADD]
        ] `shouldBe` True

testRunBytecode = do
  describe "runBytecode" $ do
    it "runBytecode [PUSH 1]" $ do
      runBytecode [PUSH 1] `shouldBe` Just 1
    it "runBytecode [PUSH 1, PUSH 2, ADD]" $ do
      runBytecode [PUSH 1, PUSH 2, ADD] `shouldBe` Just 3
    it "runBytecode [PUSH 4, PUSH 9, MULT]" $ do
      runBytecode [PUSH 4, PUSH 9, MULT] `shouldBe` Just 36
    it "runBytecode [PUSH 1, PUSH 4, PUSH 9, MULT, ADD]" $ do
      runBytecode [PUSH 1, PUSH 4, PUSH 9, MULT, ADD] `shouldBe` Just 37
    it "runBytecode [PUSH 1, PUSH 4, PUSH 9, MULT]" $ do
      runBytecode [PUSH 1, PUSH 4, PUSH 9, MULT] `shouldBe` Nothing
    it "runBytecode [PUSH 4, PUSH 1, SUB]" $ do
      runBytecode [PUSH 4, PUSH 1, SUB] `shouldBe` Just 3
    it "runBytecode [PUSH 4, PUSH 1, DIV]" $ do
      runBytecode [PUSH 4, PUSH 1, DIV] `shouldBe` Just 4

emptyEnv = M.fromList []
exampleEnv = M.fromList [("x", 1), ("y", 2), ("z", 3)]

testEvalV = do
  describe "evalV" $ do
    it "1" $ do
      evalV emptyEnv (Num 1) `shouldBe` Just 1
    it "x" $ do
      evalV emptyEnv (Var "x") `shouldBe` Nothing
    it "1 + 1" $ do
      evalV emptyEnv (Add (Num 1) (Num 1)) `shouldBe` Just 2
    it "3 * 14" $ do
      evalV emptyEnv (Mult (Num 3) (Num 14)) `shouldBe` Just 42
    it "5 - x" $ do
      evalV exampleEnv (Sub (Num 5) (Var "x")) `shouldBe` Just 4
    it "y - x" $ do
      evalV exampleEnv (Sub (Var "y") (Var "x")) `shouldBe` Just 1
    it "x + y + z" $ do
      evalV exampleEnv (Add (Add (Var "x") (Var "y")) (Var "z"))
        `shouldBe` Just 6
    it "x + y + error" $ do
      evalV exampleEnv (Add (Add (Var "x") (Var "y")) (Var "error"))
        `shouldBe` Nothing
    it "x / 0" $ do
      evalV exampleEnv (Div (Var "y") (Num 0)) `shouldBe` Nothing
