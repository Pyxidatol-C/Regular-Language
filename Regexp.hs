-- | Definition and simplification of regular expressions
module Regexp where

-- | Regular expressions
data Regexp
  = Single Char
  | SingleEmpty
  | Empty
  | Union Regexp Regexp
  | Concat Regexp Regexp
  | Star Regexp
  deriving (Eq)

instance Show Regexp where
  show r = case r of
    Single x -> [x]
    SingleEmpty -> "ε"
    Empty -> "∅"
    Union r1 r2 -> "(" ++ show r1 ++ "∪" ++ show r2 ++ ")"
    Concat r1 r2 -> "(" ++ show r1 ++ show r2 ++ ")"
    Star r -> "(" ++ show r ++ "*)"

-- | R+ is shorthand for RR*.
--
-- >>> plus (Single 'a')
-- (a(a*))
plus :: Regexp -> Regexp
plus r = r `Concat` (Star r)

-- | R^k is shorthand for the concatenation of k R's with each other.
--
-- >>> Single '0' `Regexp_.exp` 3
-- (0(00))
exp :: Regexp -> Int -> Regexp
exp r k
  | k <= 0 = error "Exponent must be strictly positive"
  | otherwise = foldr1 Concat (replicate k r)

-- | Simplify a regexp recursively.
--
-- >>> simplify $ Star Empty `Concat` Single 'a'
-- (εa)
simplify :: Regexp -> Regexp
simplify (Union Empty r) = simplify r -- ∅ acts as identity for ∪
simplify (Union r Empty) = simplify r
simplify (Union r1 r2) = Union (simplify r1) (simplify r2)
simplify (Concat SingleEmpty r) = simplify r -- ε acts as identity for ∘
simplify (Concat r SingleEmpty) = simplify r
simplify (Concat Empty _) = Empty -- ∅ acts as zero for ∘
simplify (Concat _ Empty) = Empty
simplify (Concat r1 r2) = Concat (simplify r1) (simplify r2)
simplify (Star Empty) = SingleEmpty -- (*) can only put together 0 strings from ∅
simplify (Star r) = Star (simplify r)
simplify r = r

-- | Repeatedly apply f to the argument x until the fix point is reached.
--
-- >>> repeatedly simplify $ Star Empty `Concat` Single 'a'
-- a
repeatedly :: Eq a => (a -> a) -> a -> a
repeatedly f x = g x (f x)
  where
    g x x'
      | x == x' = x
      | otherwise = g x' (f x')

rev :: Regexp -> Regexp
rev Empty = Empty
rev SingleEmpty = SingleEmpty
rev (Single a) = Single a
rev (Union x y) = Union (rev x) (rev y)
rev (Concat x y) = Concat (rev y) (rev x)
rev (Star x) = Star (rev x)
