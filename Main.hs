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
    decorateSpec = I.colourBg I.Yellow . I.colourFg I.Black
    decorateImpl = I.colourBg I.Cyan . I.colourFg I.Black
    decorateStr s
      | null s = "ε"
      | otherwise = s
    printSet s =
      let xs = S.toList s
       in mapM_ (outputStrLn . decorateStr) (take 5 xs ++ [I.colourFg I.Yellow "..." | length xs > 5])

    loop :: InputT IO ()
    loop = do
      outputStrLn "      ___                    "
      outputStrLn "     /  /\\          ___       "
      outputStrLn $ "    /  /:/_        /  /\\     " ++ " _____"
      outputStrLn $ "   /  /:/ /\\      /  /::\\    " ++ "|  __ \\"
      outputStrLn $ "  /  /:/ /:/_    /  /:/\\:\\   " ++ "| |__) |___  __ _  _____  ___ __"
      outputStrLn $ " /__/:/ /:/ /\\  /  /:/~/::\\  " ++ "|  _  // _ \\/ _` |/ _ \\ \\/ / '_ \\"
      outputStrLn $ " \\  \\:\\/:/ /:/ /__/:/ /:/\\:\\ " ++ "| | \\ \\  __/ (_| |  __/>  <| |_) |"
      outputStrLn $ "  \\  \\::/ /:/  \\  \\:\\/:/__\\/ " ++ "|_|  \\_\\___|\\__, |\\___/_/\\_\\ .__/"
      outputStrLn $ "   \\  \\:\\/:/    \\  \\::/      " ++ "             __/ |         | |"
      outputStrLn $ "    \\  \\::/      \\__\\/       " ++ "            |___/          |_|　"
      outputStrLn "     \\__\\/"
      outputStrLn ""

      outputStrLn $ I.colourFg I.Cyan "[Tip] " ++ "Write `+` for ∪ (\\cup), `$` for ε (\\epsilon), and whitespaces as you please;"
      outputStrLn $ I.colourFg I.Cyan "[Tip] " ++ "for example, `(0+10)*(1+$)` or `(0 + 10)* (1+$)`."
      outputStrLn ""

      specInput <- getInputLine $ I.colourFg I.Yellow "[?] " ++ decorateSpec "Spec" ++ "ification regexp: "
      let spec = fromJust $ R_.fromString (fromJust specInput)
      let dSpec = D_.fromRegexp (R.alphabet spec) spec
      outputStrLn $ I.colourFg I.Cyan "[!] " ++ "Constructed minimal DFA with " ++ (show . S.size . D.states) dSpec ++ " states"
      outputStrLn ""

      implInput <- getInputLine $ I.colourFg I.Yellow "[?] " ++ decorateImpl "Impl" ++ "ementation regexp: "
      let impl = fromJust $ R_.fromString (fromJust implInput)
      let dImpl' = D_.fromRegexp (R.alphabet impl) impl
      outputStrLn $ I.colourFg I.Cyan "[!] " ++ "Constructed minimal DFA with " ++ (show . S.size . D.states) dImpl' ++ " states"
      needsNegate <- getInputLine $ I.colourFg I.Yellow "[?] " ++ "Negate implementation (y/N) ? "
      let dImpl =
            if needsNegate == Just "y" || needsNegate == Just "Y"
              then D_.complement dImpl'
              else dImpl'
      outputStrLn ""

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
        outputStrLn $ I.colourFg I.Green "[!] " ++ "Equivalent"
