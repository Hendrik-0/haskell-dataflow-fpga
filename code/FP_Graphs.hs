module FP_Graphs where

import Data.List
import qualified Data.Set as S
import Data.Maybe
--import Data.Matrix
import Debug.Trace as D
--import LinSolver

type WCET = Int
type Label = Char

data Node =   HSDFNode { lb :: Label
                       , wcet :: Int
                       }
            | CSDFNode { lb :: Label
                       , wcetv :: [Int]
                       } deriving Eq


data Edge =   HSDFEdge  { ns :: Label
                        , nd :: Label
                        , tks :: Int
                        }
            | SDFEdge   { ns :: Label
                        , nd :: Label
                        , tks :: Int
                        , pr :: Int
                        , cr :: Int
                        } 
            | CSDFEdge  { ns :: Label
                        , nd :: Label
                        , prv :: [Int]
                        , crv :: [Int]
                        , tks :: Int
                        } deriving Eq

data Graph =  Graph { nodes :: [Node]
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
  fakeEdge = HSDFEdge {ns = '_', nd = r, tks = 0} -- create fake edge to root node

dfsEdges' (vs, uvs, []) = (vs, uvs, [])
dfsEdges' (vs, uvs, (e:stack)) = dfsEdges' (vs', uvs', stack') where
  efn    = edgesFromNode (nd e) uvs'  -- edges from current node (target node of current edge)
  stack' = efn ++ (stack\\[e])       -- add the edges to the stack, remove current edge from the rest of the stack to prevent double routing of already visited edges
  vs'    = vs ++ [e]                 -- add current edge to visited list
  uvs'   = uvs \\ [e]                -- remove current edge from unvisited list


resultNodes = dfsNodes hsdf 'a'
resultEdges = dfsEdges hsdf 'a'


--csdf2hsdfApprox :: Graph -> Graph
--csdf2hsdfApprox hsdf = csdf where
--  csdf = Graph {nodes = nodes hsdf}
  
--csdfEdge2hsdfEdge :: Int -> Int -> Edge -> Edge
--csdfEdge2hsdfEdge 
--f ::Fractional a => [Edge] -> [a]

--repetitionVector = solve . repetitionVector'

--repetitionVector' 
--  nodeList = (nodes gr)
--  row = replicate (length nodeList) 0
--  


--tf g = mapCol (\_ x -> negate x) nrC $ f nl m 1 el where -- negate the last column of the matrix, in order to solve the linear system
--  nl = map lb $ nodes g  -- node list containg all nodes (labels)
--  el = edges g           -- edge list containg all edges 
--  nrR = length el        -- length of the edge list is the amount of rows
--  nrC = length nl        -- length of the node list is the number of columns 
--  m = zero nrR nrC       -- initial matrix with 0
--
--f nl m j [] = m
--f nl m j (( SDFEdge {ns=ns, nd=nd, pr=pr, cr=cr}):es) = f nl m'' (j+1) es where
--  i0 = fromJust $ findIndex (==ns) nl
--  i1 = fromJust $ findIndex (==nd) nl
--  e0 = fromIntegral pr
--  e1 = fromIntegral cr
--  m'  = setElem ( e0) (j,i0+1) m
--  m'' = setElem (-e1) (j,i1+1) m' 
--
--f nl m j ((CSDFEdge {ns=ns, nd=nd, prv=prv, crv=crv}):es) = f nl m'' (j+1) es where
--  i0 = fromJust $ findIndex (==ns) nl
--  i1 = fromJust $ findIndex (==nd) nl
--  prate  = fromIntegral $ sum prv
--  crate  = fromIntegral $ sum crv
--  phaseV = fromIntegral $ length prv
--  phaseW = fromIntegral $ length crv
--  e0 = prate/phaseV
--  e1 = crate/phaseW
--  m'  = setElem ( e0) (j,i0+1) m
--  m'' = setElem (-e1) (j,i1+1) m'
--
--repetitionVector' [] = []
--repetitionVector' ((HSDFEdge {}):es) = repetitionVector' es
--repetitionVector' (( SDFEdge {ns=ns, nd=nd, pr=pr  , cr=cr  }):es) = ((ns, fromIntegral pr),(nd, fromIntegral cr)):repetitionVector' es
--repetitionVector' ((CSDFEdge {ns=ns, nd=nd, prv=prv, crv=crv}):es) = ((ns, prate/phaseV   ),(nd, crate/phaseW   )):repetitionVector' es where
--  prate  = fromIntegral $ sum prv
--  crate  = fromIntegral $ sum crv
--  phaseV = fromIntegral $ length prv
--  phaseW = fromIntegral $ length crv

sdf  = Graph {nodes = [ HSDFNode {lb = 'I', wcet = 0}
                      , HSDFNode {lb = '*', wcet = 1}
                      , HSDFNode {lb = 'f', wcet = 4}
                      , HSDFNode {lb = 'Z', wcet = 0}
                      ],
              edges = [ SDFEdge {ns = 'I', nd = '*', pr = 12, cr = 1, tks = 0}
                      , SDFEdge {ns = 'I', nd = '*', pr = 12, cr = 1, tks = 0}
                      , SDFEdge {ns = '*', nd = 'f', pr = 1 , cr = 4, tks = 0}
                      , SDFEdge {ns = 'f', nd = 'Z', pr = 1 , cr = 3, tks = 0}
                      , SDFEdge {ns = 'Z', nd = 'I', pr = 1 , cr = 1, tks = 1}
                      ]
             }

csdf = Graph {nodes = [ CSDFNode {lb = 'a', wcetv = [2,3]}
                      , CSDFNode {lb = 'b', wcetv = [2,2]}
                      , CSDFNode {lb = 'c', wcetv = [3,4]}
                      ],
              edges = [ CSDFEdge {ns = 'a', nd = 'a', prv = [1,1], crv = [1,1], tks = 1}
                      , CSDFEdge {ns = 'a', nd = 'b', prv = [1,2], crv = [1,1], tks = 0}
                      , CSDFEdge {ns = 'b', nd = 'a', prv = [2,0], crv = [2,1], tks = 2}
                      , CSDFEdge {ns = 'b', nd = 'c', prv = [1,2], crv = [2,1], tks = 0}
                      , CSDFEdge {ns = 'c', nd = 'b', prv = [2,1], crv = [1,2], tks = 3}
                      ]
             }

hsdf :: Graph
hsdf = Graph {nodes = [ HSDFNode {lb = 'a', wcet = 1}
                    , HSDFNode {lb = 'b', wcet = 1}
                    , HSDFNode {lb = 'c', wcet = 1}
                    , HSDFNode {lb = 'd', wcet = 1}
                    , HSDFNode {lb = 'e', wcet = 1}
                    , HSDFNode {lb = 'f', wcet = 1}
                    ],
            edges = [ HSDFEdge {ns = 'a', nd = 'b', tks = 0}
                    , HSDFEdge {ns = 'a', nd = 'c', tks = 0}
                    , HSDFEdge {ns = 'a', nd = 'd', tks = 0}
                    , HSDFEdge {ns = 'b', nd = 'e', tks = 0}
                    , HSDFEdge {ns = 'd', nd = 'e', tks = 0}
                    , HSDFEdge {ns = 'd', nd = 'f', tks = 0}
                    , HSDFEdge {ns = 'e', nd = 'f', tks = 0}
                    , HSDFEdge {ns = 'f', nd = 'b', tks = 0}
                    , HSDFEdge {ns = 'e', nd = 'c', tks = 0}
                    , HSDFEdge {ns = 'b', nd = 'a', tks = 0}
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

