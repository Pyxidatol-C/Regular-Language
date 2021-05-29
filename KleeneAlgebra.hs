{-# LANGUAGE TupleSections #-}

module KleeneAlgebra where

import Data.Bifunctor (first)
import qualified Data.Set as S
import GraphUtils
import Regexp

data Axiom
  = A1
  | A1'
  | A2
  | A3
  | A4
  | A5
  | A5'
  | A6
  | A6'
  | A7
  | A7'
  | A8
  | A8'
  | A9
  | A9'
  | A10
  | A10'
  | A11
  | A11'
  deriving (Bounded, Enum, Eq, Ord, Show)

rewrite :: Regexp -> S.Set (Regexp, Axiom)
rewrite r =
  S.unions
    [ S.fromList $ map (,a) (apply a r) | a <- [minBound .. maxBound]
    ]

rewriteRec :: Regexp -> S.Set (Regexp, Axiom)
rewriteRec r = case r of
  Empty -> rewrite r
  Single _ -> rewrite r
  SingleEmpty -> rewrite r
  Star x -> rewrite r `S.union` mapFst Star (rewriteRec x)
  Union x y ->
    rewrite r
      `S.union` mapFst (`Union` y) (rewriteRec x)
      `S.union` mapFst (x `Union`) (rewriteRec y)
  Concat x y ->
    rewrite r
      `S.union` mapFst (`Concat` y) (rewriteRec x)
      `S.union` mapFst (x `Concat`) (rewriteRec y)
  where
    mapFst f = S.map (first f)

apply :: Axiom -> Regexp -> [Regexp]
apply A1 (a `Union` (b `Union` c)) =
  return $ (a `Union` b) `Union` c
apply A1' ((a `Union` b) `Union` c) =
  return $ a `Union` (b `Union` c)
apply A2 (a `Union` b) =
  return $ b `Union` a
apply A3 (a `Union` a')
  | a == a' = return a
apply A4 (a `Union` Empty) =
  return a
apply A5 (a `Concat` (b `Concat` c)) =
  return $ (a `Concat` b) `Concat` c
apply A5' ((a `Concat` b) `Concat` c) =
  return $ a `Concat` (b `Concat` c)
apply A6 (a `Concat` SingleEmpty) =
  return a
apply A6' (SingleEmpty `Concat` a) =
  return a
apply A7 (a `Concat` Empty) =
  return Empty
apply A7' (Empty `Concat` a) =
  return Empty
apply A8 (a `Concat` (b `Union` c)) =
  return $ (a `Concat` b) `Union` (a `Concat` c)
apply A8' ((a `Concat` b) `Union` (a' `Concat` c))
  | a == a' = return $ a `Concat` (b `Union` c)
apply A9 ((a `Union` b) `Concat` c) =
  return $ (a `Concat` c) `Union` (b `Concat` c)
apply A9' ((a `Concat` c) `Union` (b `Concat` c'))
  | c == c' = return $ (a `Union` b) `Concat` c
apply A10 (SingleEmpty `Union` (a `Concat` Star a'))
  | a == a' = return $ Star a
apply A10' (Star a) =
  return $ SingleEmpty `Union` (a `Concat` Star a)
apply A11 (a `Concat` Star a')
  | a == a' = return $ Star a `Concat` a
apply A11' (Star a `Concat` a')
  | a == a' = return $ a `Concat` Star a
apply _ _ = []

simplify :: Int -> Regexp -> [S.Set (Regexp, [Axiom])]
simplify n r = simplify' [S.singleton (r, [])]
  where
    simplify' plys
      | length plys > n = plys
      | otherwise =
        let ply' = nextPly rewriteRec (last plys) plys
            c0 = complexity r
            reasonable r' = complexity r' <= 100 * c0
         in simplify' $ plys ++ [S.filter (reasonable . fst) ply']
