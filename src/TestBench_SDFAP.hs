module TestBench_SDFAP where

import qualified Data.Map as M
import qualified Data.List as L

import Graph
import DataFlow
import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)

dotpsdf  = Graph "dotp_sdf"   (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              , hsdfNode "f" 4
              ])
              ([SDFEdge "I" "m" 0 4 4
              , SDFEdge "m" "f" 0 4 4
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "m" "m" 1 1 1
              , SDFEdge "f" "f" 1 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

dotp = Graph 
  "dotp"
  (M.fromList
  [ hsdfNode "source" 1
  , hsdfNode "zw" 3
  , hsdfNode "fl" 3
  ])
  [ SDFAPEdge "source"  "zw" 0 [1] [2,2,2]
  , SDFAPEdge "zw"      "fl" 0 [2,2,2] [2,2,2]
  ]

s1 = Graph
  "s1"
    (M.fromList
  [ hsdfNode "A" 1
  ])
  []