module Data.Functions where

import Prelude hiding (sequence)

-- Parte 1

member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y : ys)
  | x == y = True
  | otherwise = member x ys

count :: (Num a, Eq b) => b -> [b] -> a
count x [] = 0
count x (y : ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

forall :: (a -> Bool) -> [a] -> Bool
forall f [] = True
forall f (x : xs)
  | not (f x) = False
  | otherwise = forall f xs

exists :: (a -> Bool) -> [a] -> Bool
exists f [] = False
exists f (x : xs)
  | f x = True
  | otherwise = exists f xs

first :: (a -> Bool) -> [a] -> Maybe a
first f [] = Nothing
first f (y : ys)
  | f y = Just y
  | otherwise = first f ys

single :: (a -> Bool) -> [a] -> Bool
single f l = count True (map f l) == 1

mostly :: (a -> Bool) -> [a] -> Bool
mostly f l = count True (map f l) > count False (map f l)

mostlyTrue :: [Bool] -> Bool
mostlyTrue l = count True l > count False l

-- Parte 2
majority1 :: Eq a => Int -> Maybe a -> [a] -> Maybe a
majority1 c ptr []
  | c > 0 = ptr
  | c <= 0 = Nothing
majority1 0 Nothing (x : xs) = majority1 1 (Just x) xs
majority1 0 (Just y) (x : xs) = majority1 1 (Just x) xs
majority1 c ptr (x : xs)
  | ptr == Just x = majority1 (c + 1) ptr xs
  | otherwise = majority1 (c - 1) ptr xs
majority1 _ _ _ = undefined

majority :: Eq a => [a] -> Maybe a
majority [] = Nothing
majority xs = case probable of
  Nothing -> Nothing
  Just a -> if count a xs > div (length xs) 2 then Just a else Nothing
  where
    probable = majority1 0 Nothing xs

collatz1 :: Int -> Int
collatz1 1 = 1
collatz1 n
  | even n = div n 2
  | otherwise = (3 * n) + 1

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : collatz (collatz1 n)

isFixpoint :: Eq a => (a -> a) -> a -> Bool
isFixpoint f x = f x == x

findFixpoint :: Eq a => (a -> a) -> a -> Int -> Maybe a
findFixpoint f x 0 = Nothing
findFixpoint f x limit
  | isFixpoint f x = Just x
  | otherwise = findFixpoint f (f x) (limit -1)

testCollatzConjecture :: Int -> [Int] -> [Bool]
testCollatzConjecture limit [] = []
-- testCollatzConjecture limit (x : xs) = testCollatzConjecture limit xs
testCollatzConjecture limit (x : xs) = case val of
  Nothing -> False : testCollatzConjecture limit xs
  Just a -> True : testCollatzConjecture limit xs
  where
    val = findFixpoint collatz1 x limit

tooBig :: Double -> Bool
tooBig n
  | n > 0.0001 = True
  | otherwise = False

nearlyEqual :: Double -> Double -> Bool
nearlyEqual x 0 = x < 0.0001
nearlyEqual x y
  | r <= 1.0001 && r >= 0.9999 = True
  | otherwise = False
  where
    r = x / y

-- [f(i), f(i+1), f(i+2), f(i+3), ...]
-- sequence :: (Int -> Double) -> Int -> [Double]
-- sequence f i = map f [i ..]
sequence f i = [f x | x <- [i ..]]

expand :: Int -> Int -> [Int]
expand x 0 = [x]
expand x y = x : expand (x + 1) (y -1)

-- [f(i), f(i)+f(i+1), f(i)+f(i+1)+f(i+2), f(i)+f(i+1)+f(i+2)+f(i+3), ...]
series :: (Int -> Double) -> Int -> [Double]
series f i =
  [ sum (map f (expand i steps))
    | steps <- [0 ..]
  ]

sequence' f i lim = take lim (sequence f i)

computeLn2 = computeLn2' 10001

ahs [] _ = []
ahs (x : xs) pos
  | even pos = x : ahs xs (pos + 1)
  | otherwise = - x : ahs xs (pos + 1)

computeLn2' lim
  | last seq < 0.0001 = sum $ ahs seq 0
  | otherwise = computeLn2' (lim + 1)
  where
    seq = sequence' (1 /) 1 lim

fac 0 = 1
fac n = n * fac (n - 1)

invFac 0 = 1.0
invFac n = 1 / fromInteger (fac n)

computeE = computeE' 1

computeE' lim
  | last seq < 0.0001 = sum seq
  | otherwise = computeE' (lim + 1)
  where
    seq = sequence' invFac 0 lim

computePsi = computePsi' 1

fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

invFib 0 = 1.0
invFib 1 = 1.0
invFib n = 1 / (fib (n -1) + fib (n -2))

computePsi' lim
  | last seq < 0.0001 = sum seq
  | otherwise = computePsi' (lim + 1)
  where
    seq = sequence' invFib 0 lim
