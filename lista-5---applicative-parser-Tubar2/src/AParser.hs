-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module AParser where

import Control.Applicative
import Data.Char

-- newtype Parser a where
--   P :: (String -> Maybe (a, String)) -> Parser a

newtype Parser a = P (String -> Maybe (a, String))

-- deriving (Functor)

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) s = f s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = P f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

-- Item 1

instance Functor Parser where
  fmap f (P f2) =
    P
      ( \x -> case f2 x of
          Nothing -> Nothing
          Just (x1, x2) -> Just (f x1, x2)
      )

-- Item 2

instance Applicative Parser where
  (P ft) <*> (P f) =
    P
      ( \x -> case ft x of
          Nothing -> Nothing
          Just (x1, x2) -> case f x2 of
            Nothing -> Nothing
            Just (x3, x4) -> Just (x1 x3, x4)
      )
  pure a = P (\x -> Just (a, x))

-- Item 3
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

abParser :: Parser (Char, Char)
abParser =
  P
    ( \str -> do
        let ab = take 2 str
        if ab == "ab"
          then Just (('a', 'b'), drop 2 str)
          else Nothing
    )

abParser_ :: Parser ()
abParser_ =
  P
    ( \str -> do
        let ab = take 2 str
        if ab == "ab"
          then Just ((), drop 2 str)
          else Nothing
    )

intPair :: Parser [Integer]
intPair = P f
  where
    f xs
      | l == 2 = Just (bi, "")
      | otherwise = Nothing
      where
        sp = words xs
        l = length sp
        bi = map read sp

-- Item 4

instance Alternative Parser where
  empty = P (\x -> Nothing)
  (<|>) (P f1) (P f2) =
    P
      ( \x -> case f1 x of
          Nothing -> case f2 x of
            Nothing -> Nothing
            Just (x3, x4) -> Just (x3, x4)
          Just (x1, x2) -> Just (x1, x2)
      )

-- Item 5

intOrUpperCase :: Parser ()
intOrUpperCase = P f
  where
    f str
      | isDigit (head str) = Just ((), rest)
      | isUpper (head str) = Just ((), rest2)
      | otherwise = Nothing
      where
        (ns, rest) = span isDigit str
        (ns2, rest2) = span isUpper str
