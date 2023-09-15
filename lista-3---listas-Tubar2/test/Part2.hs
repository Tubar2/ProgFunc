module Part2 where

import Prelude hiding (sequence)

import Data.Functions
import Test.Hspec

majorityTests = do
  it "majority" $ do
    majority (replicate 50 0 ++ replicate 50 1) `shouldBe` Nothing
    majority (replicate 50 0 ++ replicate 51 1) `shouldBe` Just 1
    majority (replicate 51 0 ++ replicate 50 1) `shouldBe` Just 0
    majority (replicate 100 0) `shouldBe` Just 0
    majority (replicate 1000 0) `shouldBe` Just 0
    majority (replicate 10000 0) `shouldBe` Just 0
    majority (replicate 100000 0) `shouldBe` Just 0
    majority (replicate 1000000 0) `shouldBe` Just 0
    majority ["A", "A", "A", "C", "C", "B", "B", "C", "C", "C", "B", "C", "C"] `shouldBe` Just "C"
    majority ["A", "A", "A", "A", "C", "B", "B", "C", "C", "C", "B", "C", "C"] `shouldBe` Nothing

collatz1Tests = do
  it "collatz1" $ do
    collatz1 1 `shouldBe` 1
    collatz1 2 `shouldBe` 1
    collatz1 3 `shouldBe` 10
    collatz1 4 `shouldBe` 2
    collatz1 101 `shouldBe` 304

collatzTests = do
  it "collatz" $ do
    collatz 1 `shouldBe` [1]
    collatz 2 `shouldBe` [2,1]
    collatz 3 `shouldBe` [3,10,5,16,8,4,2,1]
    collatz 4 `shouldBe` [4,2,1]
    collatz 27 `shouldBe` [27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]

isFixpointTests = do
  it "isFixpoint" $ do
    isFixpoint (\x -> 2*x) 0 `shouldBe` True
    isFixpoint (\x -> 2*x) 1 `shouldBe` False
    isFixpoint (\x -> 2*x - 1) 1 `shouldBe` True
    isFixpoint (\x -> 2*x - 1) 0 `shouldBe` False
    isFixpoint reverse [1] `shouldBe` True
    isFixpoint reverse [1,1] `shouldBe` True
    isFixpoint reverse [1,3] `shouldBe` False
    isFixpoint reverse [1,3,1] `shouldBe` True

findFixpointTests = do
  it "findFixpoint" $ do
    findFixpoint sin 0 10 `shouldBe` Just 0
    findFixpoint (\x -> x^2) 0 10 `shouldBe` Just 0
    findFixpoint (\x -> x^2 + 1) 0 10 `shouldBe` Nothing

testCollatzConjectureTests = do
  it "testCollatzConjecture" $ do
    and (testCollatzConjecture 100 [1,2,3,4,5]) `shouldBe` True
    head (testCollatzConjecture 100 [27]) `shouldBe` False
    head (testCollatzConjecture 1000 [27]) `shouldBe` True

tooBigTests = do
  it "tooBig" $ do
    tooBig 1 `shouldBe` True
    tooBig 0.1 `shouldBe` True
    tooBig 0.01 `shouldBe` True
    tooBig 0.001 `shouldBe` True
    tooBig 0.0001 `shouldBe` False
    tooBig 0.00001 `shouldBe` False
    tooBig 0.000005 `shouldBe` False

nearlyEqualTests = do
  it "nearlyEqual" $ do
    nearlyEqual 0 0 `shouldBe` True
    nearlyEqual 1 1 `shouldBe` True
    nearlyEqual 1 2 `shouldBe` False
    nearlyEqual 2 1 `shouldBe` False
    nearlyEqual 0.1 0.12 `shouldBe` False
    nearlyEqual 0.1 0.11 `shouldBe` False
    nearlyEqual 0.1 0.100001 `shouldBe` True

sequenceTests = do
  it "sequence" $ do
    take 6 (sequence linear 0) `shouldBe` [0, 1, 2, 3, 4, 5]
    take 6 (sequence doubles 0) `shouldBe` [0, 2, 4, 6, 8, 10]
    take 6 (sequence triples 0) `shouldBe` [0, 3, 6, 9, 12, 15]
    take 5 (sequence squares 0) `shouldBe` [0, 1, 4, 9, 16]
    take 100 (sequence  linear 0) `shouldBe` [0 .. 99]

seriesTests = do
  it "series" $ do
    take 5 (series linear 0) `shouldBe` [0, 1, 3, 6, 10]
    take 6 (series doubles 0) `shouldBe` [0, 2, 6, 12, 20, 30]
    take 6 (series triples 0) `shouldBe` [0, 3, 9, 18, 30, 45]
    take 5 (series squares 0) `shouldBe` [0, 1, 5, 14, 30]
    take 100 (series linear 0) `shouldBe` [n * (n+1) / 2 | n <- [0 .. 99]]

computationTests = do
  it "computation" $ do
    computeLn2 `near` 0.69314718
    computeE   `near` 2.71828182
    computePsi `near` 3.35988566

x `near` y = (x `nearlyEqual` y) `shouldBe` True

intSequence f = fromIntegral . f

linear  = intSequence id
doubles = intSequence (*2)
triples = intSequence (*3)
squares = intSequence (\n -> n * n)
