-- | Definition, construction, computation and transformation of DFAs
module DFA where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Deterministic finite automata
data DFA a = DFA
  { -- | Q - a finite set, the states
    states :: S.Set a,
    -- | Σ - a finite set, the alphabet
    alphabet :: S.Set Char,
    -- | δ: QxΣ -> Q, the transition function
    transition :: M.Map (a, Char) a,
    -- | q0 ∈ Q, the start state
    startState :: a,
    -- | F ⊆ Q, the set of accept (or final) states
    acceptStates :: S.Set a
  }

-- | Checks if the given DFA (Q, Σ, δ, q0, F) is valid:
--
-- * δ is total on QxΣ, and δ(QxΣ) ⊆ Q;
-- * q0 ∈ Q;
-- * F ⊆ Q.
isValid :: (Ord a, Show a) => DFA a -> Bool
isValid d
  | length missingKeys > 0 =
    error $ "Missing: " ++ L.intercalate ", " (map show missingKeys)
  | otherwise =
    q0 `S.member` qs
      && all (`S.member` qs) fs
      && all (`S.member` qs) values
  where
    qs = states d
    qsList = S.toList qs
    alphabetList = S.toList (alphabet d)
    δ = transition d
    q0 = startState d
    fs = acceptStates d

    keys = [(q, a) | q <- qsList, a <- alphabetList]
    missingKeys = filter (`M.notMember` δ) keys
    values = map (δ M.!) keys

-- | sample DFA recognising the language { w | every odd position of w is a 1 }
--
-- >>> dOddOnes `accepts` "10101"
-- True
--
-- >>> dOddOnes `accepts` "000"
-- False
--
-- >>> dOddOnes `accepts` ""
-- True
dOddOnes :: DFA String
dOddOnes =
  DFA
    { states = S.fromList ["even", "odd", "fail"],
      alphabet = S.fromList ['0', '1'],
      transition =
        M.fromList
          [ (("even", '0'), "fail"),
            (("even", '1'), "odd"),
            (("odd", '0'), "even"),
            (("odd", '1'), "even"),
            (("fail", '0'), "fail"),
            (("fail", '1'), "fail")
          ],
      startState = "even",
      acceptStates = S.fromList ["even", "odd"]
    }

-- | Compute the sequence of states when the DFA is computing on the string.
compute :: Ord a => DFA a -> String -> [a]
compute d = reverse . foldl f [q0]
  where
    q0 = startState d
    δ = transition d
    f qs'@(q : qs) a = δ M.! (q, a) : qs'

-- | Simulate the DFA and return whether it accepts the string.
--
-- Formally, a DFA (Q, Σ, δ, q0, F) accepts a string w = w_1...w_n if a
-- sequence of states r_0, r_1, ..., r_n in Q exists satisfying:
--
-- * r_0 = q0;
-- * δ(r_i, w_i+1) = r_i+1 for i = 0..n-1;
-- * r_n ∈ F.
accepts :: Ord a => DFA a -> String -> Bool
accepts d as =
  let q = last (d `compute` as)
   in q `S.member` acceptStates d

-- | Rename the states by applying f to obtain an equivalent/isomorphic DFA,
-- provided that f is bijective.
mapStates :: (Ord a, Ord b) => (a -> b) -> DFA a -> DFA b
mapStates f d =
  d
    { states = S.map f (states d),
      startState = f (startState d),
      transition = mapTransition f (transition d),
      acceptStates = S.map f (acceptStates d)
    }
  where
    mapFst (q, a) = (f q, a)
    mapTransition f = M.map f . M.mapKeys mapFst

-- | Get an equivalent NFA by numbering the states from 0 to #states - 1.
numberStates :: Ord a => DFA a -> DFA Int
numberStates d = mapStates number d
  where
    qsList = S.toList (states d)
    num = M.fromList $ zip qsList [0 ..]
    number = (num M.!)
