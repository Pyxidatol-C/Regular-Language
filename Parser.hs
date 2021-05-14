-- Follows the implementation from Geraint Jones's Prelims FP course closely
module Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isLower, isSpace, isUpper, ord)
import Data.List (foldl', partition)
import Data.Maybe (fromJust)

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  f `fmap` Parser p = Parser $ \xs -> [(f x, ys) | (x, ys) <- p xs]

-- | Consumes none of its input, returning a value
instance Monad Parser where
  return x = Parser $ \xs -> [(x, xs)]
  Parser p >>= f =
    Parser $ \xs -> [(v, zs) | (a, ys) <- p xs, (v, zs) <- f a `parse` ys]

instance Applicative Parser where
  pure = return
  fs <*> xs = do
    f <- fs
    x <- xs
    return (f x)

-- | Fails to parse anything
instance Alternative Parser where
  empty = Parser $ \xs -> []
  Parser p <|> Parser q = Parser $ \xs -> p xs <|> q xs

-- |
--    Deterministic choice operator;
--    parses the best match that comes from the first parser,
--    and only returns results of the second if the first fails entirely.
--    (left argument parser more specific than the right argument)
instance MonadPlus Parser

-- | The next single character, if there is one
char :: Parser Char
char =
  Parser $ \xs ->
    case xs of
      "" -> []
      (y : ys) -> return y `parse` ys

-- | Accept in a parse only a value that satisfies some predicate
(<?>) :: (a -> Bool) -> Parser a -> Parser a
c <?> p = do
  x <- p
  if c x
    then return x
    else mzero

-- | Matches exactly the given character from the input string
character :: Char -> Parser Char
character c = (== c) <?> char

-- |
--    Matches exactly the sequence of characters in a string
--    by matching first the head, and then the tail by recursion.
string :: String -> Parser String
string "" = return ""
string (c : cs) = do
  x <- character c
  xs <- string cs
  return (x : xs)

-- | Matches whitespaces
space :: Parser String
space = many (isSpace <?> char)

-- | Matches some token, followed by whitespace which is ignored
token :: Parser a -> Parser a
token p = do
  x <- p
  space
  return x

symbol :: String -> Parser String
symbol xs = token (string xs)

digit :: Parser Int
digit = value <$> (isDigit <?> char)
  where
    value x = ord x - ord '0'

digits :: Parser Int
digits = foldl shift 0 <$> token (some digit)
  where
    shift n d = 10 * n + d

-- | A number of p's, each separated by an op, and associating to the left
chain :: Parser (a -> a -> a) -> Parser a -> Parser a
chain op p = accumulate <*> p <*> (many (flip <$> op <*> p))
  where
    accumulate = pure $ foldl (flip ($))

-- | Parsess p between two symbols which act as parentheses
parens :: String -> Parser a -> String -> Parser a
parens open p close = do
  symbol open
  e <- p
  symbol close
  return e

-- |
--    Parse successful if it consumes all of the string, and is unambiguous;
--    unsuccessful if something is left over or there are 2 different possible parses.
from :: Parser a -> String -> Maybe a
from p xs =
  case [v | (v, ys) <- (space >> p) `parse` xs, null ys] of
    [v] -> Just v
    _ -> Nothing
