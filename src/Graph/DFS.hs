module Graph.DFS where

import Graph.Types

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

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
