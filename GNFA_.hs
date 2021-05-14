-- | Conversions from DFAs and NFAs
module GNFA_ where

import qualified DFA
import qualified Data.Map as M
import qualified Data.Set as S
import GNFA
import qualified NFA
import Regexp

-- | Construct the regexp on an arrow corresponding to a single letter,
-- including ε.
--
-- >>> single 'a' == Single 'a'
-- True
-- >>> single NFA.emptyStr == SingleEmpty
-- True
single :: Char -> Regexp
single a
  | a == NFA.emptyStr = SingleEmpty
  | otherwise = Single a

-- | Construct a GNFA from a DFA.
--
-- * ε arrows from the new start state to the old start state
-- * ε arrows from the old accept states to the new accept state
-- * Construct the regexp arrows, e.g. q_i ->(a,b) q_j turns into
-- q_i ->(a∪b) q_j.
fromDFA :: Ord a => DFA.DFA a -> GNFA Int
fromDFA d =
  GNFA
    { states = qs',
      transition = δ,
      startState = s,
      acceptState = f
    }
  where
    d' = DFA.numberStates d
    qs' = DFA.states d'
    δ' = DFA.transition d'
    q0' = DFA.startState d'
    fs' = DFA.acceptStates d'

    s = S.size qs'
    f = s + 1
    qsList = S.toList qs'
    arrows =
      M.unionsWith
        Union
        [ M.fromList [((s, q), Empty) | q <- qsList, q /= q0'],
          M.singleton (s, q0') SingleEmpty,
          M.fromListWith Union (map transform (M.assocs δ')),
          M.fromList [((q, f), Empty) | q <- qsList, q `S.notMember` fs'],
          M.fromList [((f0, f), SingleEmpty) | f0 <- S.toList fs']
        ]
    transform ((q1, a), q2) = ((q1, q2), single a)
    δ = fillTransition qs' s f arrows

-- | Construct a GNFA from an NFA.
-- See 'fromDFA' for a brief description of the process.
fromNFA :: Ord a => NFA.NFA a -> GNFA Int
fromNFA n =
  GNFA
    { states = qs',
      transition = δ,
      startState = s,
      acceptState = f
    }
  where
    n' = NFA.numberStates n
    qs' = NFA.states n'
    δ' = NFA.transition n'
    q0' = NFA.startState n'
    fs' = NFA.acceptStates n'

    s = S.size qs'
    f = s + 1
    qsList = S.toList qs'
    arrows =
      M.unionsWith
        Union
        [ M.fromList [((s, q), Empty) | q <- qsList, q /= q0'],
          M.singleton (s, q0') SingleEmpty,
          M.fromListWith Union (concatMap transform (M.assocs δ')),
          M.fromList [((q, f), Empty) | q <- qsList, q `S.notMember` fs'],
          M.fromList [((f0, f), SingleEmpty) | f0 <- S.toList fs']
        ]
    transform ((q, a), qs) = map (\q' -> ((q, q'), single a)) (S.toList qs)
    δ = fillTransition qs' s f arrows
