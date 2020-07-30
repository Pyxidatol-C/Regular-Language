module Regexp where

data Regexp
  = Single Char
  | Single_ε
  | Empty
  | Union Regexp Regexp
  | Concat Regexp Regexp
  | Star Regexp
  deriving (Eq)

instance Show Regexp where
  show r = case r of
    Single x -> [x]
    Single_ε -> "ε"
    Empty -> "∅"
    Union r1 r2 -> "(" ++ show r1 ++ "∪" ++ show r2 ++ ")"
    Concat r1 r2 -> "(" ++ show r1 ++ show r2 ++ ")"
    Star r -> "(" ++ show r ++ "*)"

{- Shorhtands
-}

-- | R+ is shorthand for RR*.
plus :: Regexp -> Regexp
plus r = r `Concat` (Star r)

-- | R^k is shorthand for the concatenation of k R's with each other.
exp :: Regexp -> Int -> Regexp
exp r k
  | k <= 0 = error "Exponent must be strictly positive"
  | otherwise = foldr1 Concat (replicate k r)

{- Properties
-}
simplify :: Regexp -> Regexp
simplify (Union Empty r) = simplify r -- ∅ acts as identity for ∪
simplify (Union r Empty) = simplify r
simplify (Union r1 r2) = Union (simplify r1) (simplify r2)
simplify (Concat Single_ε r) = simplify r -- ε acts as identity for ∘
simplify (Concat r Single_ε) = simplify r
simplify (Concat Empty _) = Empty -- ∅ acts as zero for ∘
simplify (Concat _ Empty) = Empty
simplify (Concat r1 r2) = Concat (simplify r1) (simplify r2)
simplify (Star Empty) = Single_ε -- (*) can only put together 0 strings from ∅
simplify (Star r) = Star (simplify r)
simplify r = r

repeatedly :: Eq a => (a -> a) -> a -> a
repeatedly f x = g x (f x)
  where
    g x x'
      | x == x' = x
      | otherwise = g x' (f x')
