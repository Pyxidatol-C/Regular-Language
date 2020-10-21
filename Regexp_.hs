-- | Operations on Regexps and conversions from DFAs and NFAs
module Regexp_ where

import qualified DFA
import qualified GNFA
import qualified GNFA_
import qualified NFA
import Regexp

-- | Convert a DFA into a regexp.
--
-- >>> fromDFA DFA.dOddOnes -- note the extra '∪ε' at the end, to be simplified
-- (((1(((1∪0)1)*))((1∪0)∪ε))∪ε)
fromDFA :: Ord a => DFA.DFA a -> Regexp
fromDFA = GNFA.convert . GNFA_.fromDFA

-- | Convert an NFA into a regexp.
--
-- >>> fromNFA NFA.nOddOnes
-- (((1(((1∪0)1)*))((1∪0)∪ε))∪ε)
fromNFA :: Ord a => NFA.NFA a -> Regexp
fromNFA = GNFA.convert . GNFA_.fromNFA
