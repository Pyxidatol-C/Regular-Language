-- | Definition, construction and reduction of GNFAs
module GNFA where

import qualified Data.Map as M
import qualified Data.Set as S
import Regexp (Regexp (Concat, Empty, Star, Union))

type Transition a = M.Map (a, a) Regexp

-- | A generalised nondeterministic finite automaton in the special form:
--
-- * 1 single start state, with regexp arrows going to every other state, but
-- no incoming arrows;
-- * 1 single accept state, with regexp arrows coming in from every other state
-- but with no outgoing arrows;
-- * regexp arrows from any state except the start state to any state except
-- the accept state - the empty set arrows can be filled with 'fillTransition'.
data GNFA a = GNFA
  { -- | A finite set of states, NOT including the start and accept states
    states :: S.Set a,
    -- | Transition function, mapping Q\{q_accept} x Q\{q_start} to a regexp
    transition :: Transition a,
    -- | The start state, no incoming arrows
    startState :: a,
    -- | The accept state, no exiting arrows
    acceptState :: a
  }

-- | Convert a GNFA to a regexp by ripping out states until only the start and
-- accept states remain.
--
-- * If the start and accept states are the only states left, then the
-- regexp on the start -> accept arrow is what we want.
-- * Otherwise find a state q_rip that is not the start or accept state, and
-- for q_i ->(R1) q_rip, q_rip ->(R2) q_rip, q_rip ->(R3) q_j, q_i ->(R4) q_j,
-- we can remove the state q_rip and replace the regexp on the q_i -> q_j arrow
-- with R1 R2* R3 ∪ R4 to get an equivalent GNFA with 1 less state.
convert :: Ord a => GNFA a -> Regexp
convert g@GNFA {states = qs, transition = δ, startState = s, acceptState = f}
  | S.null qs = δ M.! (s, f)
  | otherwise = convert g {states = qs', transition = δ'}
  where
    qRip = head (S.elems qs)
    qs' = S.delete qRip qs
    kept (q1, q2) r = q1 /= qRip && q2 /= qRip
    δ' = M.mapWithKey update (M.filterWithKey kept δ)
    -- r4 = δ M.! (q1, q2)
    update (q1, q2) r4 = r
      where
        r1 = δ M.! (q1, qRip)
        r2 = δ M.! (qRip, qRip)
        r3 = δ M.! (qRip, q2)
        r = (r1 `Concat` Star r2 `Concat` r3) `Union` r4

-- | Fill in the map of transition arrows with ∅ arrows.
fillTransition ::
  Ord a =>
  -- | The set of states, excluding the start and accept states
  S.Set a ->
  -- | The start state
  a ->
  -- | The accept state
  a ->
  -- | The transition map to be filled
  Transition a ->
  -- | The transition map filled with ∅ arrows where missing, using the fact
  -- that ∅ acts as the identity with respect to taking unions.
  Transition a
fillTransition qs s f = M.unionWith Union empty
  where
    qsList = S.toList qs
    empty = M.fromList [((q1, q2), Empty) | q1 <- s : qsList, q2 <- f : qsList]
