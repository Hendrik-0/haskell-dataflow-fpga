module Graph.BellmanFord where

import Graph.Types
import qualified Data.Map as M
import qualified Data.Set as S

{-
    Bellman -Ford shortest or longest path (change max/min for longest/shortest in bfNodeUpdate
    TODO: optimize and clean?
    The map provided by the BellmanFord function contains the distance (weight)
    to each node from the root, and the traveled path
-}
bellmanFord :: (Ord (e n), Ord n, WeightedEdges e) 
  => [e n] 
  -> n
  -> M.Map n (Weight, [e n])
bellmanFord es root
  = bellmanFord' es minit ns (S.size ns)
    where
      ns    = S.fromList (map source es ++ map target es)
      minit = M.insert root (0,[]) M.empty

bellmanFord' :: (Ord (e n), Ord n, WeightedEdges e) 
  => [e n]
  -> M.Map n (Weight, [e n]) 
  -> S.Set n
  -> Int
  -> M.Map n (Weight, [e n])
bellmanFord' es mmap ns c | mmap == mmap' = mmap
                          | c == 0        = mmap
                          | otherwise     = bellmanFord' es mmap' ns (c-1)
  where
    mmap' = foldl (bfNodeUpdate es) mmap $ ns -- 1 BellmanFord iteration

bfNodeUpdate :: (Ord (e n), Ord n, WeightedEdges e) 
  => [e n] 
  -> M.Map n (Weight, [e n]) 
  -> n
  -> M.Map n (Weight, [e n])
bfNodeUpdate es mmap n
  | val == Nothing = mmap
  | otherwise      = foldl update mmap efn
    where
      val = M.lookup n mmap
      efn = edgesFromNode n es
      Just (w,ptsn) = val -- (weight, path to source node)
--      update m e = M.insertWith min (target e) (w + weight e, ptsn ++ [e]) m -- Shortest path
      update m e = M.insertWith max (target e) (w + weight e, ptsn ++ [e]) m  -- Longest path



edgesFromNode :: (Eq n, Edges e) => n -> [e n] -> [e n]
edgesFromNode n es = filter (\e -> (source e) == n) es
