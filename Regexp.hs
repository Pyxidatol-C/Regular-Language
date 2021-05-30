-- | Definition and simplification of regular expressions
module Regexp where

import qualified Data.Set as S

-- | Regular expressions
data Regexp
  = Single Char
  | SingleEmpty
  | Empty
  | Union Regexp Regexp
  | Concat Regexp Regexp
  | Star Regexp
  deriving (Eq, Ord)

instance Show Regexp where
  show r = case r of
    Single x -> [x]
    SingleEmpty -> "ε"
    Empty -> "∅"
    Union r1 r2 -> "(" ++ show r1 ++ "∪" ++ show r2 ++ ")"
    Concat r1 r2 -> "(" ++ show r1 ++ show r2 ++ ")"
    Star r -> "(" ++ show r ++ "*)"

complexity :: Regexp -> Integer
complexity r = case r of
  Single x -> 1
  SingleEmpty -> 2
  Empty -> 5
  Union r1 r2 -> complexity r1 + complexity r2
  Concat r1 r2 -> complexity r1 * complexity r2
  Star r -> 2 ^ complexity r

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

rev :: Regexp -> Regexp
rev Empty = Empty
rev SingleEmpty = SingleEmpty
rev (Single a) = Single a
rev (Union x y) = Union (rev x) (rev y)
rev (Concat x y) = Concat (rev y) (rev x)
rev (Star x) = Star (rev x)

alphabet :: Regexp -> S.Set Char
alphabet Empty = S.empty
alphabet SingleEmpty = S.empty
alphabet (Single a) = S.singleton a
alphabet (Union x y) = S.union (alphabet x) (alphabet y)
alphabet (Concat x y) = S.union (alphabet x) (alphabet y)
alphabet (Star x) = alphabet x
