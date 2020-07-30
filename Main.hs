module Main where

import qualified DFA
import qualified DFA_
import qualified GNFA
import qualified GNFA_
import qualified NFA
import qualified NFA_
import qualified Regexp
import qualified Regexp_

main :: IO ()
main = print $ Regexp_.fromDFA DFA.dOddOnes
