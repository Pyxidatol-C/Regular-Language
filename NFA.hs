module NFA where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Transition a = M.Map (a, Char) (S.Set a)

-- | ε
emptyStr :: Char
emptyStr = 'ε'

data NFA a = NFA
  { -- | Q - a finite set, the states
    states :: S.Set a,
    -- | Σ - a finite set, the alphabet
    alphabet :: S.Set Char,
    -- | δ: Q x Σ_ε -> P(Q), the transition function
    transition :: Transition a,
    -- | q0 ∈ Q, the start state
    startState :: a,
    -- | F ⊆ Q, the set of accept (or final) states
    acceptStates :: S.Set a
  }

-- | Checks if the given NFA (Q, Σ, δ, q0, F) is valid:
--
-- * δ(Q x Σ_ε) ⊆ P(Q);
-- * q0 ∈ Q;
-- * F ⊆ Q.
isValid :: (Ord a, Show a) => NFA a -> Bool
isValid n
  | length missingKeys > 0 =
    error $ "Missing: " ++ L.intercalate ", " (map show missingKeys)
  | otherwise =
    q0 `S.member` qs
      && all (`S.member` qs) fs
      && all (`S.isSubsetOf` qs) values
  where
    qs = states n
    qsList = S.toList qs
    alphabetList = emptyStr : S.toList (alphabet n)
    δ = transition n
    q0 = startState n
    fs = acceptStates n

    keys = [(q, a) | q <- qsList, a <- alphabetList]
    missingKeys = filter (`M.notMember` δ) keys
    values = map (δ M.!) keys

-- | Fill the transition function by assigning ∅ to the missing keys of Q x Σ_ε.
--
-- For example, the total transition function for the NFA recognising the empty
-- set, with the alphabet being {a, b} (\\949 is 'emptyStr'):
--
-- >>> fillTransition (S.fromList [0]) (S.fromList ['a', 'b']) M.empty
-- fromList [((0,'a'),fromList []),((0,'b'),fromList []),((0,'\949'),fromList [])]
fillTransition :: Ord a => S.Set a -> S.Set Char -> Transition a -> Transition a
fillTransition qs as transition = M.union transition transition'
  where
    qsList = S.toList qs
    asList = emptyStr : S.toList as
    keys = [(q, a) | q <- qsList, a <- asList]
    keys' = filter (`M.notMember` transition) keys
    transition' = M.fromList [(k, S.empty) | k <- keys']

-- | sample NFA recognising the language { w | every odd position of w is a 1 }
--
-- >>> nOddOnes `accepts` "10101"
-- True
--
-- >>> nOddOnes `accepts` "000"
-- False
--
-- >>> nOddOnes `accepts` ""
-- True
nOddOnes :: NFA String
nOddOnes =
  let qs = S.fromList ["even", "odd"]
      as = S.fromList ['0', '1']
   in NFA
        { states = qs,
          alphabet = as,
          transition =
            fillTransition
              qs
              as
              ( M.fromList
                  [ (("even", '1'), S.fromList ["odd"]),
                    (("odd", '0'), S.fromList ["even"]),
                    (("odd", '1'), S.fromList ["even"])
                  ]
              ),
          startState = "even",
          acceptStates = S.fromList ["even", "odd"]
        }

-- | Compute the set of states reachable from the given set by following 0 or
-- more ε arrows.
statesReachableAlongεArrowsFrom :: Ord a => NFA a -> S.Set a -> S.Set a
statesReachableAlongεArrowsFrom n qs =
  S.unions qss
  where
    iterateUntilFix f x
      | x' == x = [x]
      | otherwise = x : iterateUntilFix f x'
      where
        x' = f x
    qss = iterateUntilFix g qs
    statesReachableFrom q = transition n M.! (q, emptyStr)
    g qs = S.unions (S.map statesReachableFrom qs)

-- | Compute the sequence of states when the NFA is computing on the string.
compute :: Ord a => NFA a -> String -> [S.Set a]
compute n = reverse . foldl f [q0']
  where
    q0 = S.singleton (startState n)
    q0' = statesReachableAlongεArrowsFrom n (q0)
    δ = transition n
    f qss'@(qs : qss) a = statesReachableAlongεArrowsFrom n qs' : qss'
      where
        qs' = S.foldr S.union S.empty (S.map g qs)
        g q = δ M.! (q, a)

-- | Simulate the NFA and return whether it accepts the string.
--
-- Formally, an NFA (Q, Σ, δ, q0, F) accepts a string w if w can be written as
-- w = y_1...y_n where each y_i ∈ Σ_ε and a sequence of states r_0, r_1, ...,
-- r_m in Q exists satisfying:
--
-- * r_0 = q0;
-- * δ(r_i, y_i+1) ∈ r_i+1 for i = 0..m-1;
-- * r_m ∈ F.
accepts :: Ord a => NFA a -> String -> Bool
accepts d as =
  let qs = last (d `compute` as)
   in any (`elem` acceptStates d) qs

-- | Rename the states by applying f to obtain an equivalent/isomorphic NFA.
mapStates :: (Ord a, Ord b) => (a -> b) -> NFA a -> NFA b
mapStates f n =
  NFA
    { states = mapSet (states n),
      alphabet = alphabet n,
      transition = mapTransition (transition n),
      startState = f (startState n),
      acceptStates = mapSet (acceptStates n)
    }
  where
    mapSet = S.map f
    mapFst (q, a) = (f q, a)
    mapTransition = M.map mapSet . M.mapKeys mapFst

-- | Get an equivalent NFA by numbering the states from 0 to #states - 1.
numberStates :: Ord a => NFA a -> NFA Int
numberStates n = mapStates number n
  where
    qs = S.toList (states n)
    num = M.fromList $ zipWith (,) qs [1 ..]
    number = (num M.!)
