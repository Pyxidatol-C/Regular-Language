import DFA qualified
import DFA_ qualified
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import NFA qualified
import NFA_ qualified

asChar :: String -> Char
asChar s = case s of
  "000" -> '0'
  "100" -> '1'
  "010" -> '2'
  "110" -> '3'
  "001" -> '4'
  "101" -> '5'
  "011" -> '6'
  "111" -> '7'

-- Automaton recognising (a + b) / 2 = c
-- where a input letter is "ai bi ci"
-- with a = a1/2 + a2/4 + ...
nWorrell =
  let qs = S.fromList ["-1", "0", "+1"]
      as = S.fromList ['0' .. '7']
   in NFA.NFA
        { NFA.states = qs
        , NFA.alphabet = as
        , NFA.transition =
            NFA.fillTransition
              qs
              as
              ( M.fromList
                  [ (("0", asChar "000"), S.fromList ["0"])
                  , (("0", asChar "111"), S.fromList ["0"])
                  , (("0", asChar "010"), S.fromList ["+1"])
                  , (("0", asChar "100"), S.fromList ["+1"])
                  , (("+1", asChar "011"), S.fromList ["+1"])
                  , (("+1", asChar "101"), S.fromList ["+1"])
                  , (("+1", asChar "001"), S.fromList ["0"])
                  , (("0", asChar "011"), S.fromList ["-1"])
                  , (("0", asChar "101"), S.fromList ["-1"])
                  , (("-1", asChar "010"), S.fromList ["-1"])
                  , (("-1", asChar "100"), S.fromList ["-1"])
                  , (("-1", asChar "110"), S.fromList ["0"])
                  ]
              )
        , NFA.startState = "0"
        , NFA.acceptStates = S.fromList ["0"]
        }

-- see https://pyxidatol-c.github.io/Logic-and-Proof-slajds/Sheet3/?step=33
nYang =
  let qs = S.fromList ["init", "=0;+0", "=0;+1", "=1;+0", "=1;+1"]
      as = S.fromList ['0' .. '7']
   in NFA.NFA
        { NFA.states = qs
        , NFA.alphabet = as
        , NFA.transition =
            NFA.fillTransition
              qs
              as
              ( M.fromList
                  [ (("init", NFA.emptyStr), S.fromList ["=0;+0", "=1;+1"])
                  , (("=0;+0", asChar "000"), S.fromList ["=0;+0", "=1;+1"])
                  , (("=0;+0", asChar "010"), S.fromList ["=1;+0"])
                  , (("=0;+0", asChar "100"), S.fromList ["=1;+0"])
                  , (("=0;+1", asChar "010"), S.fromList ["=0;+1"])
                  , (("=0;+1", asChar "100"), S.fromList ["=0;+1"])
                  , (("=0;+1", asChar "110"), S.fromList ["=0;+0", "=1;+1"])
                  , (("=1;+0", asChar "001"), S.fromList ["=0;+0", "=1;+1"])
                  , (("=1;+0", asChar "011"), S.fromList ["=1;+0"])
                  , (("=1;+0", asChar "101"), S.fromList ["=1;+0"])
                  , (("=1;+1", asChar "011"), S.fromList ["=0;+1"])
                  , (("=1;+1", asChar "101"), S.fromList ["=0;+1"])
                  , (("=1;+1", asChar "111"), S.fromList ["=0;+0", "=1;+1"])
                  ]
              )
        , NFA.startState = "init"
        , NFA.acceptStates = S.fromList ["=0;+0"]
        }

isEquiv = DFA_.whyNotEquivalent (DFA_.fromNFA' nYang) (DFA_.fromNFA' nWorrell)
  