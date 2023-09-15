{-# LANGUAGE GADTs #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

type Ident = String

data Atom where
  N :: Integer -> Atom
  I :: Ident -> Atom
  deriving (Eq, Show)

data SExpr where
  A :: Atom -> SExpr
  Comb :: [SExpr] -> SExpr
  deriving (Eq, Show)

-- Item 6
zeroOrMoreAux :: (String -> Maybe (a, String)) -> String -> Maybe ([a], String)
zeroOrMoreAux f str = undefined

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (P f1) = P (zeroOrMoreAux f1)

-- zeroOrMore (P f) =
--   P
--     ( \x -> case f x of
--         Nothing -> Just ([], x)
--         Just (x1, x2) -> case f x2 of
--           Nothing -> _
--           Just x3 -> _
--     )

oneOrMore :: Parser a -> Parser [a]
oneOrMore (P f) =
  P
    ( \x -> case f x of
        Nothing -> Nothing
        Just (x1, x2) -> Just ([x1], x2)
    )

-- Item 7

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

-- Item 8

parseSExpr :: Parser SExpr
parseSExpr = undefined
