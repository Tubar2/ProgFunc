-- from https://github.com/ekmett/linear/blob/master/src/Linear/Epsilon.hs

module Epsilon where

class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool
  (≈) :: a -> a -> Bool
  x ≈ y = nearZero $ x - y

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12
