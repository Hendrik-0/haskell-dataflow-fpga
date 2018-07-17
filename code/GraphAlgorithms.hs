module GraphAlgorithms where

import GraphTypes
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio

{-
    A depth-first ordering or edges reachable from a given node.
    let (unreachable, reachable) = dfs xs root
    then unreachable and reachable form a partitioning of xs, with
    the following properties:

    - unreachable is the list of edges of xs that do not lie on
      any path that starts in root, and only uses edges from xs.
    - if (a, b), (c, d) are consecutive elements in reachable,
      then either b == c or there is an edge (x, y) with x == b that preceeds
      (a, b).

    Note that if edges composes a strongly connected graph, then unreachable is empty
-}

dfsHO :: (Edges e, Eq n) => [e n] -> n -> ([e n], [e n])
dfsHO edges root
  = foldl dfs' (ys, []) xs
    where
      isOutgoing edge    = source edge == root
      (xs, ys)           = partition isOutgoing edges
      dfs' (es, vs) edge = let (es', path) = dfsHO es (target edge) in (es', vs ++ edge:path)

{-
    Same as dfsHO, but now ignoring the orientation of edges
-}
dfsU :: (Edges e, Eq n) => [e n] -> n -> ([e n], [e n])
dfsU edges root
  = foldl dfs' (ys, []) xs
    where
      incident edge        = source edge == root || target edge == root
      (xs, ys)             = partition incident edges
      dfs' (es, vs) edge
        | source edge == root = let (es', path) = dfsU es (target edge) in (es', vs ++ edge:path)
        | otherwise       = let (es', path) = dfsU es (source edge) in (es', vs ++ edge:path)

{- 
    depth first search on general undirected graphs
-}
dfsGU :: (Edges e, Eq n) => [e n] -> [e n]
dfsGU [] = []
dfsGU (e:es)
  = path ++ dfsGU es'
    where
      root = source e
      (es', path) = dfsU (e:es) root


{-
    Bellman -Ford shortest,  TODO: optimize and clean?
    The map provided by the BellmanFord function contains the distance (weight)
    to each node from the root, and the traveled path
-}
bellmanFord 
  :: WeightedGraph 
  -> Label 
  -> M.Map Label (Weight, [Edge Label])
bellmanFord graph root
--  = (iterate (bfIteration graph) minit)!!l
  = bellmanFord' graph minit l
    where
      minit = M.insert root (0,[]) M.empty
      l = length (nodes graph)

bellmanFord' 
  :: WeightedGraph
  -> M.Map Label (Weight, [Edge Label])
  -> Int
  -> M.Map Label (Weight, [Edge Label])
bellmanFord' graph mmap c | mmap == mmap' = mmap
                          | c == 0        = mmap
                          | otherwise     = bellmanFord' (graph :: WeightedGraph) mmap' (c-1)
  where
    mmap' = foldl (bfNodeUpdate (edges graph)) mmap $ M.keys (nodes graph) -- 1 BellmanFord iteration

bfNodeUpdate 
  :: (Ord (e n), Ord n, WeightedEdges e) 
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
      update m e = M.insertWith min (target e) (w + weight e, ptsn ++ [e]) m




edgesFromNode :: (Eq n, Edges e) => n -> [e n] -> [e n]
edgesFromNode n es = filter (\e -> (source e) == n) es

edgesToNode :: (Eq n, Edges e) => n -> [e n] -> [e n]
edgesToNode n es = filter (\e -> (target e) == n) es

edgesOnNode :: (Eq n, Edges e) => n -> [e n] -> [e n]
edgesOnNode n es = filter (\e -> (source e) == n || (target e) == n) es


{-
dfsEdgesR gr r 
  = tail visited                                     --  use tail to throw away fake edge
    where
      (visited,_,_) = dfsEdgesR' ([], (edges gr), [fakeEdge])
      fakeEdge = (Edge '_' r) -- create fake edge to root node

dfsEdgesR' :: (Edges e, Eq (e n), Eq n) => ([e n], [e n], [e n]) -> ([e n], [e n], [e n])
dfsEdgesR' (vs, uvs, []) = (vs, uvs, [])
dfsEdgesR' (vs, uvs, (e:stack)) 
  = dfsEdgesR' (vs', uvs', stack') 
    where
      efn    = edgesFromNode (target e) uvs'  -- edges from current node (target node of current edge)
      stack' = efn ++ (stack\\[e])       -- add the edges to the stack, remove current edge from the rest of the stack to prevent double routing of already visited edges
      vs'    = vs ++ [e]                 -- add current edge to visited list
      uvs'   = uvs \\ [e]                -- remove current edge from unvisited list
-}


