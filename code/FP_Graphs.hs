module FP_Graphs where

import Data.List
import qualified Data.Set as S

type WCET = Int
type Label = Char

data Node = Node  { lb :: Label
                  , wcet :: Int
                  } deriving Eq
data Edge = Edge  { ns :: Label
                  , nd :: Label
                  , tks :: Int
                  } deriving Eq
data Graph = Graph  { nodes :: [Node]
                    , edges :: [Edge]
                    }


edgesFromNode :: Label -> [Edge] -> [Edge]
edgesFromNode n es = filter (\e -> (ns e) == n) es

dfsNodes gr r = visited where
  (visited,_,_) = dfsNodes' gr ([], unvisited, [r])
  unvisited = map lb $ nodes gr

dfsNodes' gr (vs, uvs, []) = (vs, uvs, [])
dfsNodes' gr (vs, [] , stack) = (vs,[], stack)
dfsNodes' gr (vs, uvs, (n:stack)) =  dfsNodes' gr (vs', uvs', stack') where
  efn    = edgesFromNode n (edges gr)  -- all outgoing edges from current node
  dcn    = map (nd) efn                -- directly connected nodes
  dcn'   = dcn \\ vs                   -- remove already visited nodes
  stack' = dcn' ++ (stack\\[n])        -- add unvisited, reachable nodes to stack, and remove current node from stack
  vs'    = vs ++ [n]                   -- add current node to visited list
  uvs'   = uvs \\ [n]                  -- remove current node from unvisited list

dfsEdges gr r = tail visited where --  use tail to throw away fake edge
  (visited,_,_) = dfsEdges' ([], (edges gr), [fakeEdge])
  fakeEdge = Edge {ns = '_', nd = r, tks = 0} -- create fake edge to root node

dfsEdges' (vs, uvs, []) = (vs, uvs, [])
dfsEdges' (vs, uvs, (e:stack)) = dfsEdges' (vs', uvs', stack') where
  efn    = edgesFromNode (nd e) uvs  -- edges from current node (target node of current edge)
  stack' = efn ++ (stack\\[e])       -- add the edges to the stack, remove current edge from the rest of the stack to prevent double routing of already visited edges
  vs'    = vs ++ [e]                 -- add current edge to visited list
  uvs'   = uvs \\ [e]                -- remove current edge from unvisited list


resultNodes = dfsNodes gr 'a'
resultEdges = dfsEdges gr 'a'

gr :: Graph
gr = Graph { nodes = [ Node {lb = 'a', wcet = 1}
                     , Node {lb = 'b', wcet = 1}
                     , Node {lb = 'c', wcet = 1}
                     , Node {lb = 'd', wcet = 1}
                     , Node {lb = 'e', wcet = 1}
                     , Node {lb = 'f', wcet = 1}
                     ],
            edges = [ Edge {ns = 'a', nd = 'b', tks = 0}
                    , Edge {ns = 'a', nd = 'c', tks = 0}
                    , Edge {ns = 'a', nd = 'd', tks = 0}
                    , Edge {ns = 'b', nd = 'e', tks = 0}
                    , Edge {ns = 'd', nd = 'e', tks = 0}
                    , Edge {ns = 'd', nd = 'f', tks = 0}
                    , Edge {ns = 'e', nd = 'f', tks = 0}
                    , Edge {ns = 'f', nd = 'b', tks = 0}
                    , Edge {ns = 'e', nd = 'c', tks = 0}
                    , Edge {ns = 'b', nd = 'a', tks = 0}
                    ]
           }

instance Show Edge where
  show e = [(ns e)] ++ "--->" ++ [(nd e)]

instance Show Node where
  show n = [lb n]

  
-- partially working
--dfs g root = dfs' ([], edges g) (Edge {ns = '_', nd = root, tks = 0})
--
--dfs' (vs, uvs) edge = foldl dfs' (vs', uvs') eft where
--  eft = edgesFromNode (nd edge) uvs
--  vs' = vs ++ [edge]
--  uvs' = uvs\\[edge]

--dfs g root = dfs' ([], edges g) (Edge {ns = '_', nd = root, tks = 0})
--
--dfs' (vs, uvs) edge = (vs'', uvs'') where
--  eft = edgesFromNode (nd edge) uvs'
--  vs' = vs ++ [edge]
--  uvs' = uvs\\[edge]
--  (vs'', uvs'') = foldl dfs' (vs', uvs') eft

