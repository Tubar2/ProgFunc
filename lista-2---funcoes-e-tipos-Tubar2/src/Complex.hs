module Complex where

import Epsilon

data Complex = Rect Double Double | Polar Double Double

isRect :: Complex -> Bool
isRect (Rect _ _) = True
isRect (Polar _ _) = False

isPolar :: Complex -> Bool
isPolar (Rect _ _) = False
isPolar (Polar _ _) = True

polarMag :: Complex -> Double
polarMag (Rect a b) = sqrt ((a * a) + (b * b))
polarMag (Polar a _) = a

polarAng :: Complex -> Double
polarAng (Rect a b) = atan (b / a) * (180 / pi)
polarAng (Polar _ b) = b

toPolar :: Complex -> Complex
toPolar a = Polar (polarMag a) (polarAng a)

realPart :: Complex -> Double
realPart (Rect a _) = a
realPart (Polar a b) = a * cos (b * pi / 180)

imagPart :: Complex -> Double
imagPart (Rect _ b) = b
imagPart (Polar a b) = a * sin (b * pi / 180)

toRect :: Complex -> Complex
toRect a = Rect (realPart a) (imagPart a)

-- As funções abaixo permitem que comparemos
-- membros do tipo Complex.
instance Eq Complex where
  (Rect x y) == (Rect x' y') = x ≈ x' && y ≈ y'
  p1@(Polar _ _) == p2@(Polar _ _) = toRect p1 == toRect p2
  c@(Rect _ _) == p@(Polar _ _) = c == toRect p
  p@(Polar _ _) == c@(Rect _ _) = c == toRect p

-- Defina as operações da typeclass Num
-- para o tipo Complex.
instance Num Complex where
  (+) (Rect a b) (Rect c d) = Rect (a + c) (b + d)
  (+) a b = (+) (toRect a) (toRect b)

  (*) (Rect a b) (Rect c d) = Rect (a * c - b * d) (a * d + b * c)
  (*) a b = (*) (toRect a) (toRect b)

  abs (Polar a b) = Polar a 0
  abs a = abs $ toPolar a

  signum (Polar _ b) = Polar 1 b
  signum a = signum $ toPolar a

  fromInteger n = Rect (fromInteger n) 0

  negate (Rect a b) = Rect (negate a) (negate b)
  negate c = negate $ toRect c

-- Defina a função show para um parâmetro Complex.
-- Essa função converte um elemento para uma string.
instance Show Complex where
  show (Polar a b) = show a ++ "#" ++ show b
  show (Rect a 0) = show a
  show (Rect a b)
    | b < 0 = show a ++ " - " ++ show (abs b) ++ "i"
    | otherwise = show a ++ " + " ++ show (abs b) ++ "i"
