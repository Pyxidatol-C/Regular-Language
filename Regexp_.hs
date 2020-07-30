-- | Operations on Regexps and conversions from DFAs and NFAs
module Regexp_ where

import qualified DFA
import qualified GNFA
import qualified GNFA_
import qualified NFA
import Regexp

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

-- | Convert a DNA into a regexp.
--
-- >>> fromDFA DFA.dOddOnes -- note the extra '∪ε' at the end, to be simplified
-- (((1(((1∪0)1)*))((1∪0)∪ε))∪ε)
fromDFA :: Ord a => DFA.DFA a -> Regexp
fromDFA = GNFA.convert . GNFA_.fromDFA

-- | Convert an NFA into a regexp.
-- >>> fromNFA NFA.nOddOnes
-- (((1(((1∪0)1)*))((1∪0)∪ε))∪ε)
fromNFA :: Ord a => NFA.NFA a -> Regexp
fromNFA = GNFA.convert . GNFA_.fromNFA
