module GraphAlgorithms where

import GraphTypes
import Data.List
import qualified Data.Map as M

edgesFromNode :: Label -> [Edge] -> [Edge]
edgesFromNode n es = filter (\e -> (ns e) == n) es

edgesToNode :: Label -> [Edge] -> [Edge]
edgesToNode n es = filter (\e -> (nd e) == n) es

edgesOnNode :: Label -> [Edge] -> [Edge]
edgesOnNode n es = filter (\e -> (nd e) == n || (ns e) == n) es

{-
--dfsNodesGUR :: Graph -> [Nodes]
--dfsNodesGUR gr [] = []
--dfsNodesGUR gr = 
--  path ++ dfsNodesGUR gr uvs
--    where
--      (path, uvs,_) = dfsNodesR' gr Undirected ([],(e:es), ns e)

dfsNodesR :: Graph -> Label -> [Label]
dfsNodesR gr r 
  = visited 
    where
      (visited,_,_) = dfsNodesR' Directed gr ([], unvisited, [r])
      unvisited = M.keys (nodes gr) 

dfsNodesR' :: Graph -> GraphDir -> ([Label],[Label],[Label]) -> ([Label], [Label], [Label])
dfsNodesR' gr _ (vs, uvs, []) = (vs, uvs, [])
dfsNodesR' gr _ (vs, [] , stack) = (vs,[], stack)
dfsNodesR' gr graphDir (vs, uvs, (n:stack)) 
  = dfsNodesR' gr graphDir (vs', uvs', stack') 
    where
      efn   | graphDir == Directed = edgesFromNode n (edges gr)  -- all outgoing edges from current node
            | otherwise            = edgesOnNode n (edges gr)  -- all outgoing edges from current node
      dcn    = map (nd) efn                -- directly connected nodes
      dcn'   = dcn \\ vs                   -- remove already visited nodes
      stack' = dcn' ++ (stack\\[n])        -- add unvisited, reachable nodes to stack, and remove current node from stack
      vs'    = vs ++ [n]                   -- add current node to visited list
      uvs'   = uvs \\ [n]                  -- remove current node from unvisited list
-}

dfsEdgesR :: Graph -> Label -> [Edge]
dfsEdgesR gr r 
  = tail visited                                     --  use tail to throw away fake edge
    where
      (visited,_,_) = dfsEdgesR' ([], (edges gr), [fakeEdge])
      fakeEdge = HSDFEdge {ns = '_', nd = r, tks = 0} -- create fake edge to root node

dfsEdgesR' :: ([Edge], [Edge], [Edge]) -> ([Edge],[Edge],[Edge])
dfsEdgesR' (vs, uvs, []) = (vs, uvs, [])
dfsEdgesR' (vs, uvs, (e:stack)) 
  = dfsEdgesR' (vs', uvs', stack') 
    where
      efn    = edgesFromNode (nd e) uvs'  -- edges from current node (target node of current edge)
      stack' = efn ++ (stack\\[e])       -- add the edges to the stack, remove current edge from the rest of the stack to prevent double routing of already visited edges
      vs'    = vs ++ [e]                 -- add current edge to visited list
      uvs'   = uvs \\ [e]                -- remove current edge from unvisited list

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
dfsHO :: [Edge] -> Label -> ([Edge], [Edge])
dfsHO edges root
  = foldl dfs' (ys, []) xs
    where
      isOutgoing edge    = ns edge == root
      (xs, ys)           = partition isOutgoing edges
      dfs' (es, vs) edge = let (es', path) = dfsHO es (nd edge) in (es', vs ++ edge:path)

{-
    Same as dfsHO, but now ignoring the orientation of edges
-}
dfsU :: [Edge] -> Label -> ([Edge], [Edge])
dfsU edges root
  = foldl dfs' (ys, []) xs
    where
      incident edge        = ns edge == root || nd edge == root
      (xs, ys)             = partition incident edges
      dfs' (es, vs) edge
        | ns edge == root = let (es', path) = dfsU es (nd edge) in (es', vs ++ edge:path)
        | otherwise       = let (es', path) = dfsU es (ns edge) in (es', vs ++ edge:path)

{- 
    depth first search on general undirected graphs
-}
dfsGU :: [Edge] -> [Edge]
dfsGU [] = []
dfsGU (e:es)
    = path ++ dfsGU es'
    where
      root = ns e
      (es', path) = dfsU (e:es) root

