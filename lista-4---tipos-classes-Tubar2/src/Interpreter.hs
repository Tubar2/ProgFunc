module Interpreter where

import qualified Data.Map as M
import Parser
import Syntax

-- 1

div' :: Int -> Int -> Maybe Int
div' _ 0 = Nothing
div' a b = Just (div a b)

eval :: Expr -> Maybe Int
eval (Num a) = Just a
eval (Add a b) = do
  c <- eval a
  d <- eval b
  Just (c + d)
eval (Sub a b) = do
  c <- eval a
  d <- eval b
  Just (c - d)
eval (Mult a b) = do
  c <- eval a
  d <- eval b
  Just (c * d)
eval (Div a b) = do
  c <- eval a
  d <- eval b
  div' c d
eval _ = Nothing

-- 2
evalStr :: String -> Maybe Int
evalStr a = do
  case (parseString a) of
    Nothing -> Nothing
    Just ex -> eval ex

-- 6
type Env = M.Map String Int

evalV :: Env -> Expr -> Maybe Int
evalV m (Var a) = M.lookup a m
evalV _ (Num a) = Just a
evalV m (Add a b) = do
  c <- evalV m a
  d <- evalV m b
  Just (c + d)
evalV m (Sub a b) = do
  c <- evalV m a
  d <- evalV m b
  Just (c - d)
evalV m (Mult a b) = do
  c <- evalV m a
  d <- evalV m b
  Just (c * d)
evalV m (Div a b) = do
  c <- evalV m a
  d <- evalV m b
  div' c d

-- evalV _ _ = Nothing
