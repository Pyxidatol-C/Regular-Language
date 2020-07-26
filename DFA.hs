module DFA where

import qualified Data.List as L
import qualified Data.Map as M

data DFA a = DFA
  { -- | Q - a finite set, the states
    states :: [a],
    -- | Σ - a finite set, the alphabet
    alphabet :: [Char],
    -- | δ: QxΣ -> Q, the transition function
    transition :: M.Map (a, Char) a,
    -- | q0 ∈ Q, the start state
    startState :: a,
    -- | F ⊆ Q, the set of accept (or final) states
    acceptStates :: [a]
  }

-- | Checks if the given DFA (Q, Σ, δ, q0, F) is valid:
-- * δ is total on QxΣ, and δ(QxΣ) ⊆ Q;
-- * q0 ∈ Q;
-- * F ⊆ Q.
isValid :: (Ord a, Show a) => DFA a -> Bool
isValid d =
  q0 `elem` qs
    && all (`elem` qs) fs
    && all (`elem` qs) values
  where
    qs = states d
    δ = transition d
    q0 = startState d
    fs = acceptStates d

    keys = [(q, a) | q <- qs, a <- alphabet d]
    missingKeys = filter (`M.notMember` δ) keys
    values =
      if length missingKeys == 0
        then map (δ M.!) keys
        else error $ "Missing: " ++ L.intercalate ", " (map show missingKeys)

-- | sample DFA recognising the language { w | every odd position of w is a 1 }
dOddOnes :: DFA String
dOddOnes =
  DFA
    { states = ["even", "odd", "fail"],
      alphabet = ['0', '1'],
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
      acceptStates = ["even", "odd"]
    }

-- | Compute the sequence of states when the DFA is computing on the string.
compute :: Ord a => DFA a -> String -> [a]
compute d = reverse . foldl f [q0]
  where
    q0 = startState d
    δ = transition d
    f qs'@(q : qs) a = δ M.! (q, a) : qs'

-- | Simulate the DFA and return whether it accepts the string.
accepts :: Ord a => DFA a -> String -> Bool
accepts d as =
  let q = last (d `compute` as)
   in q `elem` acceptStates d
