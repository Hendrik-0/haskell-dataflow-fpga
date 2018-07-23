module TestBench where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple

import DataFlow
import Graph

wgraph = Graph (M.fromList
              [('S', Node 'S')
              ,('A', Node 'A')
              ,('B', Node 'B')
              ,('C', Node 'C')
              ,('D', Node 'D')
              ,('E', Node 'E')
              ])
              ([WeightedEdge 'S' 'A' 10
              , WeightedEdge 'S' 'E' 8
              , WeightedEdge 'E' 'D' 1
              , WeightedEdge 'D' 'A' (-4)
              , WeightedEdge 'D' 'C' (-1)
              , WeightedEdge 'A' 'C' 2
              , WeightedEdge 'C' 'B' (-2)
              , WeightedEdge 'B' 'A' 1
              ])

-- example graphs:
sdf  = Graph (M.fromList 
              [ ('I', HSDFNode 'I' 0)
              , ('*', HSDFNode '*' 1)
              , ('f', HSDFNode 'f' 4)
              , ('Z', HSDFNode 'Z' 0)
              ])
              ([SDFEdge 'I' '*' 0 12 1
              , SDFEdge 'I' '*' 0 12 1
              , SDFEdge '*' 'f' 0 1  4
              , SDFEdge 'f' 'Z' 0 1  3
              , SDFEdge 'Z' 'I' 1 1  1
              ])
             
csdf = Graph (M.fromList                      -- modulus = 36
              [ ('a', CSDFNode 'a' [2,3])         -- q = 4
              , ('b', CSDFNode 'b' [2,2])         -- q = 6
              , ('c', CSDFNode 'c' [3,4])         -- q = 6
              ])
              ([CSDFEdge 'a' 'a' 1 [1,1] [1,1]    -- weight = 9
              , CSDFEdge 'a' 'b' 0 [1,2] [1,1]    -- weight = 6
              , CSDFEdge 'b' 'a' 2 [2,0] [2,1]    -- weight = 6
              , CSDFEdge 'b' 'c' 0 [1,2] [2,1]    -- weight = 4
              , CSDFEdge 'c' 'b' 3 [2,1] [1,2]    -- weight = 4
              ])

hsdf = Graph (M.fromList
              [('a', HSDFNode 'a' 1)
              ,('b', HSDFNode 'b' 1)
              ,('c', HSDFNode 'c' 1)
              ,('d', HSDFNode 'd' 1)
              ,('e', HSDFNode 'e' 1)
              ,('f', HSDFNode 'f' 1)
              ])
              ([HSDFEdge 'a' 'b' 0
              , HSDFEdge 'a' 'c' 0
              , HSDFEdge 'a' 'd' 0
              , HSDFEdge 'b' 'e' 0
              , HSDFEdge 'd' 'e' 0
              , HSDFEdge 'd' 'f' 0
              , HSDFEdge 'e' 'f' 0
              , HSDFEdge 'f' 'b' 0
              , HSDFEdge 'e' 'c' 0
              , HSDFEdge 'b' 'a' 0
              ])




hsdf2 = Graph (M.fromList
              [('b', HSDFNode 'b' 3)
              ,('d', HSDFNode 'd' 9)
              ,('a', HSDFNode 'a' 1)
              ,('c', HSDFNode 'c' 5)
              ])
              ([HSDFEdge 'b' 'd' 1
              , HSDFEdge 'b' 'c' 1
              , HSDFEdge 'b' 'a' 0
              , HSDFEdge 'a' 'a' 1
              , HSDFEdge 'a' 'b' 1
              , HSDFEdge 'a' 'c' 1
              , HSDFEdge 'd' 'c' 1
              , HSDFEdge 'c' 'a' 1
              ])

hsdf3 = Graph (M.fromList
              [('a', HSDFNode 'a' 15)
              ,('b', HSDFNode 'b' 2)
              ,('c', HSDFNode 'c' 1)
              ,('d', HSDFNode 'd' 1)
              ,('e', HSDFNode 'e' 1)
              ,('f', HSDFNode 'f' 1)
              ])
              ([HSDFEdge 'a' 'b' 0
              , HSDFEdge 'a' 'c' 0
              , HSDFEdge 'a' 'd' 0
              , HSDFEdge 'a' 'a' 0
              , HSDFEdge 'b' 'e' 0
              , HSDFEdge 'd' 'e' 0
              , HSDFEdge 'd' 'f' 0
              , HSDFEdge 'e' 'f' 0
              , HSDFEdge 'f' 'b' 1
              , HSDFEdge 'e' 'c' 0
              --, HSDFEdge 'b' 'a' 0
              ])

