module StdLogic where

data StdLogic = T | F | X | Z

instance Eq StdLogic where
  (==) T T = True
  (==) F F = True
  (==) X X = True
  (==) Z Z = True
  (==) _ _ = False

instance Show StdLogic where
  show T = "T"
  show F = "F"
  show X = "X"
  show Z = "Z"

stdNot :: StdLogic -> StdLogic
stdNot X = X
stdNot F = T
stdNot T = F
stdNot Z = X

stdAnd :: StdLogic -> StdLogic -> StdLogic
stdAnd X F = F
stdAnd X _ = X
stdAnd F _ = F
stdAnd T T = T
stdAnd T F = F
stdAnd T _ = X
stdAnd Z F = F
stdAnd Z _ = X

stdOr :: StdLogic -> StdLogic -> StdLogic
stdOr X T = T
stdOr X _ = X
stdOr F F = F
stdOr F T = T
stdOr F _ = X
stdOr T _ = T
stdOr Z T = T
stdOr Z _ = X

stdXor :: StdLogic -> StdLogic -> StdLogic
stdXor X _ = X
stdXor Z _ = X
stdXor F F = F
stdXor F T = T
stdXor F _ = X
stdXor T F = T
stdXor T T = F
stdXor T _ = X

adder :: StdLogic -> StdLogic -> StdLogic -> (StdLogic, StdLogic)
adder b1 b2 cin = (s, cout)
  where
    s = stdXor cin (stdXor b1 b2)
    cout = stdOr (stdAnd b1 b2) (stdAnd cin (stdXor b1 b2))

mux :: StdLogic -> StdLogic -> StdLogic -> StdLogic
mux a b s = stdOr (stdAnd a (stdNot s)) (stdAnd b s)
