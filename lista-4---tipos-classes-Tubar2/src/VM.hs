{-# LANGUAGE TypeSynonymInstances #-}

module VM where

import Data.Foldable (Foldable (fold))
import Display
import qualified Interpreter
import Syntax

data Bytecode
  = PUSH Int
  | ADD
  | SUB
  | MULT
  | DIV
  deriving (Eq, Show)

-- 4
compile :: Expr -> [Bytecode]
compile (Num a) = [PUSH a]
compile (Add a b) = (compile a) ++ (compile b) ++ [ADD]
compile (Sub a b) = (compile a) ++ (compile b) ++ [SUB]
compile (Mult a b) = (compile a) ++ (compile b) ++ [MULT]
compile (Div a b) = (compile a) ++ (compile b) ++ [DIV]

-- 5

runByteAcc :: [Bytecode] -> [Bytecode] -> Maybe Int
runByteAcc [] [x] =
  case x of
    PUSH a -> Just a
    _ -> Nothing
runByteAcc [] [_, _] = Nothing
runByteAcc acc (PUSH a : PUSH b : c : xs) = do
  case c of
    ADD -> runByteAcc [] (PUSH (a + b) : acc ++ xs)
    SUB -> runByteAcc [] (PUSH (a - b) : acc ++ xs)
    MULT -> runByteAcc [] (PUSH (a * b) : acc ++ xs)
    DIV -> runByteAcc [] (PUSH (div a b) : acc ++ xs)
    PUSH d -> runByteAcc (PUSH a : acc) (PUSH b : c : xs)

runBytecode :: [Bytecode] -> Maybe Int
runBytecode = runByteAcc []
