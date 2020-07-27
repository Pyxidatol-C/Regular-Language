module NFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Transition a = M.Map (a, Char) (S.Set a)

-- | ε
emptyStr :: Char
emptyStr = 'ε'

data NFA a = NFA
  { -- | Q - a finite set, the states
    states :: [a],
    -- | Σ - a finite set, the alphabet
    alphabet :: [Char],
    -- | δ: Q x Σ_ε -> P(Q), the transition function
    transition :: Transition a,
    -- | q0 ∈ Q, the start state
    startState :: a,
    -- | F ⊆ Q, the set of accept (or final) states
    acceptStates :: [a]
  }

-- | Checks if the given NFA (Q, Σ, δ, q0, F) is valid:
-- * δ(Q x Σ_ε) ⊆ P(Q);
-- * q0 ∈ Q;
-- * F ⊆ Q.
isValid :: (Ord a, Show a) => NFA a -> Bool
isValid n =
  q0 `elem` qs
    && all (`elem` qs) fs
    && all (`S.isSubsetOf` qSet) values
  where
    qs = states n
    qSet = S.fromList qs
    δ = transition n
    q0 = startState n
    fs = acceptStates n

    keys = [(q, a) | q <- qs, a <- emptyStr : alphabet n]
    missingKeys = filter (`M.notMember` δ) keys
    values =
      if length missingKeys > 0
        then error $ "Missing: " ++ L.intercalate ", " (map show missingKeys)
        else map (δ M.!) keys

fromList :: Ord a => [a] -> [Char] -> [((a, Char), a)] -> Transition a
fromList qs as = foldr f e
  where
    e = M.fromList [((q, a), S.empty) | q <- qs, a <- emptyStr : as]
    f ((q, a), q') m = M.adjust (S.insert q') (q, a) m

-- | sample NFA recognising the language { w | every odd position of w is a 1 }
nOddOnes :: NFA String
nOddOnes =
  let qs = ["even", "odd"]
      as = ['0', '1']
   in NFA
        { states = qs,
          alphabet = as,
          transition =
            fromList
              qs
              as
              [ (("even", '1'), "odd"),
                (("odd", '0'), "even"),
                (("odd", '1'), "even")
              ],
          startState = "even",
          acceptStates = ["even", "odd"]
        }

-- | Compute the sequence of states when the NFA is computing on the string.
compute :: Ord a => NFA a -> String -> [S.Set a]
compute d = reverse . foldl f [S.singleton q0]
  where
    q0 = startState d
    δ = transition d
    f qss'@(qs : qss) a = S.foldr S.union S.empty (S.map g qs) : qss'
      where
        g q = δ M.! (q, a)

-- | Simulate the NFA and return whether it accepts the string.
-- Formally, an NFA (Q, Σ, δ, q0, F) accepts a string w if w can be written as
-- w = y_1...y_n where each y_i ∈ Σ_ε and a sequence of states r_0, r_1, ...,
-- r_m in Q exists satisfying:
-- * r_0 = q0;
-- * δ(r_i, y_i+1) ∈ r_i+1 for i = 0..m-1;
-- * r_m ∈ F.
accepts :: Ord a => NFA a -> String -> Bool
accepts d as =
  let qs = last (d `compute` as)
   in any (`elem` acceptStates d) qs
