module Interface where

import qualified Data.Set as S
import qualified NFA as N
import qualified NFA_ as N_
import qualified Regexp as R

data Colour
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  deriving (Enum, Eq, Ord)

_fg :: Colour -> Int
_fg c
  | c <= White = 30 + (fromEnum c - fromEnum Black)
  | c >= BrightBlack = 90 + (fromEnum c - fromEnum BrightBlack)

_bg :: Colour -> Int
_bg c
  | c <= White = 40 + (fromEnum c - fromEnum Black)
  | c >= BrightBlack = 100 + (fromEnum c - fromEnum BrightBlack)

colourFg :: Colour -> String -> String
colourFg c s = "\ESC[" ++ show (_fg c) ++ "m" ++ s ++ "\ESC[0m"

colourBg :: Colour -> String -> String
colourBg c s = "\ESC[" ++ show (_bg c) ++ "m" ++ s ++ "\ESC[0m"

probeRegexp :: Int -> R.Regexp -> [String]
probeRegexp n r = ("Strings accepted by " ++ show r ++ ":") : map decorate (take n sigmaStarred)
  where
    sigma = S.toList (R.alphabet r)
    sigmaStarred = concat $ iterate prefixAll [""]
    prefixAll ss = [a : s | a <- sigma, s <- ss]
    nfa = N_.fromRegexp (S.fromList sigma) r
    decorate s
      | N.accepts nfa s = colourFg Green s'
      | otherwise = colourFg Red s'
      where
        s' = if null s then "ε" else s
