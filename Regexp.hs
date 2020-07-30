-- | Definition and simplification of regular expressions
module Regexp where

-- | Regular expressions
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

-- | Simplify a regexp recursively.
--
-- >>> simplify $ Star Empty `Concat` Single 'a'
-- (εa)
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
