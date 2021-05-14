module ParserRegexp where

import Control.Applicative
import Data.Char (isAlpha, isDigit)
import qualified Parser as P
import qualified Regexp as R
import Prelude hiding (concat)

regexp, concat, star, symbol :: P.Parser R.Regexp
regexp = unionOp `P.chain` concat
concat = concatOp `P.chain` (star <|> symbol)
star = do
  r <- symbol
  P.symbol "*"
  return $ R.Star r
symbol = symbolParser <|> P.parens "(" regexp ")"
  where
    symbolParser = do
      s <- isSymbol P.<?> P.char
      return $ if s == emptyStrSymbol then R.SingleEmpty else R.Single s

isSymbol :: Char -> Bool
isSymbol c = any ($ c) [isAlpha, isDigit, (== emptyStrSymbol)]

emptyStrSymbol :: Char
emptyStrSymbol = '$'

unionOp, concatOp :: P.Parser (R.Regexp -> R.Regexp -> R.Regexp)
unionOp = R.Union <$ P.symbol "+"
concatOp = R.Concat <$ P.symbol ""

parseRegexp :: String -> Maybe R.Regexp
parseRegexp = P.from regexp
