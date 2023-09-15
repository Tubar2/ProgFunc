module Part1 where

import Data.Functions
import Test.Hspec

memberTests = do
  it "member" $ do
    member (1 :: Int)  ([1,2,3,4]:: [Int]) `shouldBe` True
    member (0 :: Int)  ([1,2,3,4]:: [Int]) `shouldBe` False
    member (2 :: Int)  ([1,2,3,4]:: [Int]) `shouldBe` True
    member (50 :: Int) ([1..100] :: [Int])  `shouldBe` True

countTests = do
  it "count" $ do
    count (1 :: Int)  [1,2,3,4] `shouldBe` 1
    count (0 :: Int)  [1,2,3,4] `shouldBe` 0
    count (40 :: Int) [1..100] `shouldBe` 1
    count (1 :: Int)  (replicate 100 1) `shouldBe` 100

forallTests = do
  it "forall" $ do
    forall even [1,2,3,4] `shouldBe` False
    forall odd [1,2,3,4] `shouldBe` False
    forall even [2,4,6,8] `shouldBe` True
    forall odd [1,3,5,7] `shouldBe` True
    forall (== 1) (replicate 100 1) `shouldBe` True
    forall (== 1) (0 :(replicate 100 1)) `shouldBe` False
    forall (== 1) ((replicate 100 1) ++ [0]) `shouldBe` False

existsTests = do
  it "exists" $ do
    exists even [1,2,3,4] `shouldBe` True
    exists odd [1,2,3,4] `shouldBe` True
    exists even [2,4,6,8] `shouldBe` True
    exists odd [1,3,5,7] `shouldBe` True
    exists even [2,4,6,8,11] `shouldBe` True
    exists odd [1,3,5,7,8] `shouldBe` True
    exists (== 1) (replicate 100 1) `shouldBe` True
    exists (== 1) (replicate 100 0) `shouldBe` False
    exists (== 1) (0 :(replicate 100 1)) `shouldBe` True
    exists (== 1) ((replicate 100 0) ++ [1]) `shouldBe` True

firstTests = do
  it "first" $ do
    first even [1,2,3,4] `shouldBe` Just 2
    first odd [1,2,3,4] `shouldBe` Just 1
    first even [2,4,6,8] `shouldBe` Just 2
    first odd [1,3,5,7] `shouldBe` Just 1
    first odd [2,4,6,8,11] `shouldBe` Just 11
    first even [1,3,5,7,8] `shouldBe` Just 8
    first odd [2,4,6,8] `shouldBe` Nothing
    first even (replicate 100 1) `shouldBe` Nothing
    first even (0 :(replicate 100 1)) `shouldBe` Just 0
    first odd ((replicate 100 0) ++ [1]) `shouldBe` Just 1

singleTests = do
  it "single" $ do
    single even [1,2,3,4] `shouldBe` False
    single odd [1,2,3,4] `shouldBe` False
    single even [2,4,6,8] `shouldBe` False
    single odd [1,3,5,7] `shouldBe` False
    single odd [2,4,6,8,11] `shouldBe` True
    single even [1,3,5,7,8] `shouldBe` True
    single odd [2,4,6,8] `shouldBe` False
    single even (replicate 100 1) `shouldBe` False
    single even (0 :(replicate 100 1)) `shouldBe` True
    single odd ((replicate 100 0) ++ [1]) `shouldBe` True

mostlyTests = do
  it "mostly" $ do
    mostly even [1,2,3,4] `shouldBe` False
    mostly odd [1,2,3,4] `shouldBe` False
    mostly even [2,4,6,8] `shouldBe` True
    mostly odd [1,3,5,7] `shouldBe` True
    mostly odd [2,4,6,8,11] `shouldBe` False
    mostly even [1,3,5,7,8] `shouldBe` False
    mostly even [2,4,6,8,11] `shouldBe` True
    mostly odd [1,3,5,7,8] `shouldBe` True
    mostly even (replicate 50 0 ++ replicate 51 1) `shouldBe` False
    mostly odd (replicate 50 0 ++ replicate 51 1) `shouldBe` True

mostlyTrueTests = do
  it "mostlyTrue" $ do
    mostlyTrue [True] `shouldBe` True
    mostlyTrue [True, False] `shouldBe` False
    mostlyTrue [True, False, True] `shouldBe` True
    mostlyTrue (replicate 50 True ++ replicate 51 False) `shouldBe` False
    mostlyTrue (replicate 51 True ++ replicate 51 False) `shouldBe` False
