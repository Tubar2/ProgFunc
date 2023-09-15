module Syntax where

import Display

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

-- 3
instance Display Expr where
  display (Num a) = show a
  display (Add a b) = "(" ++ display a ++ " + " ++ display b ++ ")"
  display (Sub a b) = "(" ++ display a ++ " - " ++ display b ++ ")"
  display (Mult a b) = "(" ++ display a ++ " * " ++ display b ++ ")"
  display (Div a b) = "(" ++ display a ++ " / " ++ display b ++ ")"
  display (Var a) = a
