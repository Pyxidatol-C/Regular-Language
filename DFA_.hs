module DFA_ where

import DFA
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified NFA as NFA
import qualified NFA_ as NFA_
import qualified Regexp as R

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

fromRegexp :: S.Set Char -> R.Regexp -> DFA (S.Set Int)
fromRegexp alphabet' = fromNFA . (NFA_.fromRegexp alphabet')
