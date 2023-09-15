module Main where

import Part1
import Part2

import Test.Hspec

main :: IO ()
main = hspec $ do
  part1tests
  part2tests

part1tests = do
  describe "Parte-1" $ do
    memberTests
    countTests
    forallTests
    existsTests
    firstTests
    singleTests
    mostlyTests
    mostlyTrueTests


part2tests = do
  describe "Parte-2" $ do
    majorityTests
    collatz1Tests
    collatzTests
    isFixpointTests
    findFixpointTests
    testCollatzConjectureTests
    tooBigTests
    nearlyEqualTests
    sequenceTests
    seriesTests
    computationTests
