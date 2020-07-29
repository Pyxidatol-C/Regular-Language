module NFA_ where

import qualified DFA
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Regexp as R
import NFA

-- | Convert a DFA into an NFA - every DFA is (almost) automatically an NFA.
fromDFA :: Ord a => DFA.DFA a -> NFA a
fromDFA d =
  NFA
    { states = DFA.states d,
      alphabet = DFA.alphabet d,
      transition = transition',
      startState = DFA.startState d,
      acceptStates = DFA.acceptStates d
    }
  where
    delta = M.map S.singleton (DFA.transition d)
    transition' = fillTransition (DFA.states d) (DFA.alphabet d) delta

-- | Construct the union of the 2 NFAs.
--
-- Formally, for NFAs N1 and N2 recognising the languages A and B, the union of
-- the NFAs recognise the language A∪B = { x | x ∈ A or x ∈ B }.
--
-- Idea: Add a new start state with ε arrows leading to the two original start
-- states, so that the machine nondeterministically "guesses" which NFA to use.
-- The accept states are those of either of the automata.
union :: Ord a => NFA a -> NFA a -> NFA Int
union n1 n2
  | alphabet n1 /= alphabet n2 = error "Different alphabets"
  | otherwise =
    NFA
      { states = states',
        alphabet = alphabet',
        transition = transition',
        startState = q0',
        acceptStates = acceptStates'
      }
  where
    n1' = numberStates n1
    n2' = mapStates (+ size1) (numberStates n2)
    size1 = S.size (states n1') -- max (states n1') + 1
    size2 = S.size (states n2')

    states' = S.singleton q0' `S.union` states n1' `S.union` states n2'
    alphabet' = alphabet n1 -- same as alphabet n2
    emptyStrArrows =
      M.singleton (q0', emptyStr) (S.fromList [startState n1', startState n2'])
    transition' =
      fillTransition
        states'
        alphabet'
        -- there shouldn't be any duplicate keys
        (M.unions [emptyStrArrows, transition n1', transition n2'])
    q0' = size1 + size2 -- max (states n2') + 1
    acceptStates' = acceptStates n1' `S.union` acceptStates n2'

-- | Construct the concatenation of the NFAs, the first followed by the second.
--
-- Formally, for NFAs N1 and N2 recognising the languages A and B, the
-- concatenation of the NFAs recognise the language
-- A∘B = { xy | x ∈ A, y ∈ B }.
--
-- Idea: Add ε arrows from the accept states of N1 to the start state of n2, so
-- that the machine nondeterministically "guesses" where the language of N1
-- ends. The accept states are those of N2.
concat :: Ord a => NFA a -> NFA a -> NFA Int
concat n1 n2
  | alphabet n1 /= alphabet n2 = error "Different alphabets"
  | otherwise =
    NFA
      { states = states',
        alphabet = alphabet',
        transition = transition',
        startState = q0',
        acceptStates = acceptStates'
      }
  where
    n1' = numberStates n1
    n2' = mapStates (+ size1) (numberStates n2)
    size1 = S.size (states n1') -- max (states n1') + 1
    size2 = S.size (states n2') -- max (states n1') + 1
    states' = states n1' `S.union` states n2'
    alphabet' = alphabet n1 -- same as alphabet n2
    emptyStrArrows =
      M.fromList
        [ ((f, emptyStr), S.singleton (startState n2'))
          | f <- S.toList (acceptStates n1')
        ]
    transition' =
      fillTransition
        states'
        alphabet'
        -- Here we need to union with the ε arrows in the original accept state
        (M.unionsWith S.union [emptyStrArrows, transition n1', transition n2'])
    q0' = startState n1'
    acceptStates' = acceptStates n2'

-- | Construct the star of the NFA, i.e. the language occuring 0 or more times.
--
-- Formally, for an NFA N recognising the language A, the star of the NFA
-- recognise the language A* = { x_1x_2...x_k | k ≥ 0, ∀i x_i ∈ A }.
--
-- Idea: Add a new accepting start state to recognise the empty string, with an
-- ε arrow leading to the original start state. The accept states are those of
-- N, with ε arrows leading back to the original start state,
-- nondeterministically "guessing" where each x_i ends.
star :: Ord a => NFA a -> NFA Int
star n =
  NFA
    { states = states',
      alphabet = alphabet',
      transition = transition',
      startState = q0',
      acceptStates = acceptStates'
    }
  where
    n' = numberStates n

    states' = S.singleton q0' `S.union` states n'
    alphabet' = alphabet n'
    q0'_q0 = ((q0', emptyStr), S.singleton (startState n'))
    fs_q0 =
      [ ((f, emptyStr), S.singleton (startState n'))
        | f <- S.toList (acceptStates n')
      ]
    emptyStrArrows = M.fromList (q0'_q0 : fs_q0)
    transition' =
      fillTransition
        states'
        alphabet'
        -- Here we need to union with the ε arrows in the original accept state
        (M.unionWith S.union emptyStrArrows (transition n'))
    q0' = S.size (states n') -- max (states n') + 1
    acceptStates' = S.singleton q0' `S.union` acceptStates n'

-- | Construct an NFA recognising the language of the regular expression.
--
-- >>> (fromRegexp (S.fromList "ab") R.Empty) `accepts` ""
-- False
--
-- >>> (fromRegexp (S.fromList "ab") R.Single_ε) `accepts` ""
-- True
--
-- >>> (fromRegexp (S.fromList "ab") (R.plus (R.Single 'a'))) `accepts` ""
-- False
-- >>> (fromRegexp (S.fromList "ab") (R.plus (R.Single 'a'))) `accepts` "aaaaa"
-- True
--
-- >>> (fromRegexp (S.fromList "ab") (R.Star (R.Single 'a'))) `accepts` "aaaaa"
-- True
--
-- >>> (fromRegexp (S.fromList "ab") ((R.Single 'a') `R.exp` 2)) `accepts` "aa"
-- True
-- >>> (fromRegexp (S.fromList "ab") ((R.Single 'a') `R.exp` 2)) `accepts` "a"
-- False
fromRegexp :: S.Set Char -> R.Regexp -> NFA Int
fromRegexp alphabet' r = case r of
  R.Single c ->
    let q0 = 0
        q' = 1 -- Accept state
        qs = S.fromList [q0, q']
        qs' = S.singleton q'
        delta = M.singleton (q0, c) qs'
     in NFA
          { states = qs,
            alphabet = alphabet',
            transition = fillTransition qs alphabet' delta,
            startState = q0,
            acceptStates = qs'
          }
  R.Single_ε ->
    let q0 = 0
        qs = S.singleton q0
     in NFA
          { states = qs,
            alphabet = alphabet',
            transition = fillTransition qs alphabet' M.empty,
            startState = q0,
            acceptStates = qs
          }
  R.Empty ->
    let q0 = 0
        qs = S.singleton q0
     in NFA
          { states = qs,
            alphabet = alphabet',
            transition = fillTransition qs alphabet' M.empty,
            startState = q0,
            acceptStates = S.empty
          }
  R.Union r1 r2 -> fromRegexp alphabet' r1 `union` fromRegexp alphabet' r2
  R.Concat r1 r2 ->
    fromRegexp alphabet' r1 `NFA_.concat` fromRegexp alphabet' r2
  R.Star r' -> star (fromRegexp alphabet' r')
