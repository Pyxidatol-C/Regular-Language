{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import qualified DFA as D
import qualified DFA_ as D_
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Interface as I
import KleeneAlgebra
import qualified Regexp as R
import qualified Regexp_ as R_
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    decorateSpec = I.colourBg I.Green . I.colourFg I.Black
    decorateImpl = I.colourBg I.Red . I.colourFg I.White
    decorateStr s
      | null s = "ε"
      | otherwise = s
    printSet s =
      let xs = S.toList s
       in mapM_ (outputStrLn . decorateStr) (take 5 xs ++ [I.colourFg I.Yellow "..." | length xs > 5])

    loop :: InputT IO ()
    loop = do
      outputStrLn "\ESC[32m      ___                    "
      outputStrLn "\ESC[32m     /  /\\          ___       "
      outputStrLn $ "\ESC[32m    /  /:/_        /  /\\     " ++ "\ESC[31m _____"
      outputStrLn $ "\ESC[32m   /  /:/ /\\      /  /::\\    " ++ "\ESC[31m|  __ \\"
      outputStrLn $ "\ESC[32m  /  /:/ /:/_    /  /:/\\:\\   " ++ "\ESC[31m| |__) |___  __ _  _____  ___ __"
      outputStrLn $ "\ESC[32m /__/:/ /:/ /\\  /  /:/~/::\\  " ++ "\ESC[31m|  _  // _ \\/ _` |/ _ \\ \\/ / '_ \\"
      outputStrLn $ "\ESC[32m \\  \\:\\/:/ /:/ /__/:/ /:/\\:\\ " ++ "\ESC[31m| | \\ \\  __/ (_| |  __/>  <| |_) |"
      outputStrLn $ "\ESC[32m  \\  \\::/ /:/  \\  \\:\\/:/__\\/ " ++ "\ESC[31m|_|  \\_\\___|\\__, |\\___/_/\\_\\ .__/"
      outputStrLn $ "\ESC[32m   \\  \\:\\/:/    \\  \\::/      " ++ "\ESC[31m             __/ |         | |"
      outputStrLn $ "\ESC[32m    \\  \\::/      \\__\\/       " ++ "\ESC[31m            |___/          |_|　"
      outputStrLn "\ESC[32m     \\__\\/"
      outputStrLn "\ESC[32m"

      outputStrLn $ I.colourFg I.Cyan "[Tip] " ++ "Write `+` for ∪ (\\cup), `$` for ε (\\epsilon), and spaces as you please;"
      outputStrLn $ I.colourFg I.Cyan "[Tip] " ++ "for example, `(0+10)*(1+$)` or `(0 + 10)* (1+$)`."
      outputStrLn ""

      specInput <- getInputLine $ I.colourFg I.Yellow "[?] " ++ decorateSpec "Spec" ++ "ification regexp:\ESC[34m  "
      let spec = fromJust $ R_.fromString (fromJust specInput)
      let dSpec = D_.fromRegexp (R.alphabet spec) spec
      outputStrLn $ I.colourFg I.Cyan "[.] " ++ "Constructed minimal DFA with " ++ (show . S.size . D.states) dSpec ++ " states"
      outputStrLn "\ESC[0m"

      implInput <- getInputLine $ I.colourFg I.Yellow "[?] " ++ decorateImpl "Impl" ++ "ementation regexp:\ESC[34m "
      let impl = fromJust $ R_.fromString (fromJust implInput)
      let dImpl' = D_.fromRegexp (R.alphabet impl) impl
      outputStrLn $ I.colourFg I.Cyan "[.] " ++ "Constructed minimal DFA with " ++ (show . S.size . D.states) dImpl' ++ " states"
      needsNegate <- getInputLine $ I.colourFg I.Yellow "[?] " ++ "Negate implementation (y/N) ?\ESC[34m "
      let dImpl =
            if needsNegate == Just "y" || needsNegate == Just "Y"
              then D_.complement dImpl'
              else dImpl'
      outputStrLn "\ESC[0m"

      let diffs = D_.whyNotEquivalent dSpec dImpl

      unless (S.null (fst diffs)) $
        do
          outputStrLn $ I.colourFg I.Red "[!] " ++ "Strings accepted by " ++ decorateSpec "Spec" ++ " \\ " ++ decorateImpl "Impl" ++ ": "
          printSet $ fst diffs
          outputStrLn ""

      unless (S.null (snd diffs)) $
        do
          outputStrLn $ I.colourFg I.Red "[!] " ++ "Strings accepted by " ++ decorateImpl "Impl" ++ " \\ " ++ decorateSpec "Spec" ++ ": "
          printSet $ snd diffs
          outputStrLn ""

      when (S.null (uncurry S.union diffs)) $
        do
          outputStrLn $ I.colourFg I.Green "[.] " ++ "Equivalent"
          outputStrLn ""
