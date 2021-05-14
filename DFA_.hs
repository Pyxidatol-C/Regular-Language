-- | Operations on DFAs and conversion from NFAs
module DFA_ where

import DFA
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified NFA
import qualified NFA_
import qualified Regexp as R

-- | Construct the complement the DFA.
--
-- Formally, given a DFA D recognising the language A, the complement D'
-- recognises the language A^C = { w | w ∈ Σ*, w ∉ A }.
--
-- >>> (complement dOddOnes) `accepts` "101"
-- False
--
-- >>> (complement dOddOnes) `accepts` "0"
-- True
complement :: Ord a => DFA a -> DFA a
complement d = d {acceptStates = states d S.\\ acceptStates d}

-- | Construct the intersection of the 2 DFAs.
--
-- Formally, given DFAs D1 and D2 recognising the languages A1 and A2, the
-- interesction of D1 and D2 recognise the language A1 ∩ A2.
intersect :: (Ord a, Ord b) => DFA a -> DFA b -> DFA (a, b)
intersect d1 d2
  | alphabet d1 /= alphabet d2 = error "Different alphabets"
  | otherwise =
    DFA
      { states = states',
        alphabet = alphabet',
        transition = transition',
        startState = startState',
        acceptStates = acceptStates'
      }
  where
    states' = S.cartesianProduct (states d1) (states d2)
    statesList = S.toList states'
    alphabet' = alphabet d1
    alphabetList = S.toList alphabet'
    δ1 = transition d1
    δ2 = transition d2
    transition' =
      M.fromList
        [ ((q, a), (δ1 M.! (q1, a), δ2 M.! (q2, a)))
          | q@(q1, q2) <- statesList,
            a <- alphabetList
        ]
    startState' = (startState d1, startState d2)
    acceptStates' = S.filter isAccept states'
    isAccept (q1, q2) =
      q1 `S.member` acceptStates d1
        && q2 `S.member` acceptStates d2

-- | Construct a DFA that is equivalent to the NFA.
--
-- Idea: Given an NFA N = (Q, Σ, δ, q0, F), the set of states of the equivalent
-- DFA D is the power set of Q; the start state is the set of states reachable
-- from q0 following 0 or more ε arrows; the accept states are the sets which
-- contain a member of F; the transition function keeps track of the set of
-- states that the NFA would be in.
fromNFA :: Ord a => NFA.NFA a -> DFA (S.Set a)
fromNFA n =
  DFA
    { states = states',
      alphabet = alphabet',
      transition = transition',
      startState = startState',
      acceptStates = acceptStates'
    }
  where
    states' = S.powerSet (NFA.states n)
    statesList = S.toList states'
    alphabet' = NFA.alphabet n
    alphabetList = S.toList alphabet'
    reachε = NFA.statesReachableAlongεArrowsFrom n
    delta = NFA.transition n
    transition' =
      M.fromList
        [ ((qs, a), S.unions images)
          | qs <- statesList,
            a <- alphabetList,
            let image q = delta M.! (q, a),
            let images = S.map (reachε . image) qs
        ]
    q0 = S.singleton (NFA.startState n)
    startState' = reachε q0
    isAcceptInNFA = (`S.member` NFA.acceptStates n)
    acceptStates' = S.filter (any isAcceptInNFA) states'

fromNFA' :: Ord a => NFA.NFA a -> DFA (S.Set (S.Set a))
fromNFA' = minimize . fromNFA

equivalent :: (Ord a, Ord b) => DFA a -> DFA b -> Bool
equivalent d1 d2 =
  DFA.isLanguageEmpty d1Diffd2 && DFA.isLanguageEmpty d2Diffd1
  where
    d1Diffd2 = d1 `intersect` complement d2
    d2Diffd1 = d2 `intersect` complement d1

fromRegexp :: S.Set Char -> R.Regexp -> DFA (S.Set (S.Set Int))
fromRegexp alphabet r = case r of
  R.Empty -> toDFA r
  R.SingleEmpty -> toDFA r
  R.Single _ -> toDFA r
  R.Union x y -> fromNFA' (fromRegexp' x `NFA_.union` fromRegexp' y)
  R.Concat x y -> fromNFA' (fromRegexp' x `NFA_.concat` fromRegexp' y)
  R.Star x -> fromNFA' (NFA_.star (fromRegexp' x))
  where
    toDFA = fromNFA' . NFA_.fromRegexp alphabet
    fromRegexp' = NFA_.fromDFA . fromRegexp alphabet
