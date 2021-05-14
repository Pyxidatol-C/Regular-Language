module GraphUtils where

import qualified Data.Set as S

bfs :: (Ord a, Ord q) => q -> (q -> S.Set (q, a)) -> S.Set (q, [a])
bfs q0 next = S.unions plys
  where
    plys = bfsPlys q0 next

bfsPlys :: (Ord a, Ord q) => q -> (q -> S.Set (q, a)) -> [S.Set (q, [a])]
bfsPlys q0 next =
  let explore (ply0 : plys) =
        if S.null ply0
          then plys
          else explore $ nextPly next ply0 plys : ply0 : plys
   in explore [S.singleton (q0, [])]

nextPly :: (Ord a, Ord q) => (q -> S.Set (q, a)) -> S.Set (q, [a]) -> [S.Set (q, [a])] -> S.Set (q, [a])
nextPly next ply0 plys =
  let nextPly' _ [] = []
      nextPly' qs ((q, as) : vs) =
        let neighbours =
              [ (q', a' : as)
                | (q', a') <- S.toList $ next q,
                  all (S.notMember q' . S.map fst) plys,
                  q' `notElem` qs
              ]
            qs' = map fst neighbours
         in neighbours ++ nextPly' (qs' ++ qs) vs
   in S.fromList $ nextPly' [] (S.toList ply0)
