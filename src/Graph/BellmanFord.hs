module Graph.BellmanFord where

import Graph.Types
import qualified Data.Map as M
import qualified Data.Set as S

{-
    Bellman -Ford shortest or longest path (change max/min for longest/shortest in bfNodeUpdate
    TODO: optimize and clean?
    TODO: remove Bool isCycle from M.Map???
    Thei BellmanFord function provides Either a
    a M.Map containing (weight, path to node, is a cycle) for each node,
    or a list of cycles, which is a list of paths in which each paths forms a cycle.
-}
bellmanFord :: (Ord (e l), Ord l, WeightedEdges e) 
  => [e l]
  -> l
  -> Either [[e l]] (M.Map l (Weight, [e l]))
bellmanFord es root
  = bellmanFord' es minit ns (S.size ns)
    where
      ns    = S.fromList (map source es ++ map target es)
      minit = M.insert root (0,[],False) M.empty -- note that the starting M.Map also contains a Boolean if the path is a cycle.

{-
    bellmanFord' iterates the bellmanFord steps it produces Either:
    a M.Map containing (weight, path to node, is a cycle) for each node,
    or a list cycles, which is a list paths.
-}
bellmanFord' :: (Ord (e l), Ord l, WeightedEdges e) 
  => [e l]
  -> M.Map l (Weight, [e l], Bool) 
  -> S.Set l
  -> Int
  -> Either [[e l]] (M.Map l (Weight, [e l])) 
bellmanFord' es mmap ns c | mmap == mmap' = Right $ M.map (\(a,b,_) -> (a,b)) mmap -- remove the boolean from the M.Map, so only provide the weight and path
                          | hasCycles     = Left $ M.elems cycles -- $ map (\(_,p,_) -> p) $ M.elems cycles
--                          | c == 0 && mmap' /= mmap = Nothing
                          | c < 0        = error "Bellmanford error"
                          | otherwise     = bellmanFord' es mmap' ns (c-1)
  where
    mmap' = foldl (bfNodeUpdate es) mmap $ ns -- 1 BellmanFord iteration
    cycles = M.mapWithKey removeNonCyclicPart $ M.filter (\(_,_,b) -> b) mmap'
    hasCycles = length cycles > 0
    removeNonCyclicPart lbl (w, path, _) = dropWhile (\edge -> source edge /= lbl) path

{-
    bfNodeUpdate looks at all the outgoing edges from a node n, and updates the mmap 
    if the outgoing edges reach nodes with a shorter/longer path.
    the node, paths, and ifCycle is stored in the M.Map
-}
bfNodeUpdate :: (Ord (e l), Ord l, WeightedEdges e) 
  => [e l] 
  -> M.Map l (Weight, [e l], Bool) 
  -> l
  -> M.Map l (Weight, [e l], Bool)
bfNodeUpdate es mmap n
  | val == Nothing = mmap
  | otherwise      = foldl update mmap efn
    where
      val             = M.lookup n mmap
      efn             = edgesFromNode n es -- all edges from the source node.
      Just (w,ptsn,_) = val                -- (weight, path to source node, and isCycle bool)
--      update m e = M.insertWith min (target e) (w', ptsn', isCycle) m -- Shortest path
      update m e = M.insertWith max' (target e) (w', ptsn', isCycle) m  -- Longest path
        where
          isCycle    = (target e) `elem` (map source ptsn ++ map target ptsn) || isSelfEdge -- if there is a self edge, the path is empty, so check explicitly
          isSelfEdge = target e == source e
          ptsn'      =  ptsn ++ [e]    -- path to current node is path to source node + edge to current node
          w'         = w + weight e
          max' s@(w1, _, _) t@(w2, _, _) = if w1 > w2 then s else t     -- insertWith max' compares new value s with old value t

edgesFromNode :: (Eq l, Edges e) => l -> [e l] -> [e l]
edgesFromNode n es = filter (\e -> (source e) == n) es
