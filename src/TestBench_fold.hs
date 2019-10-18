module TestBench_fold where

import qualified Data.Map as M
import DataFlow
import Graph
import Hardware

import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)

hs  = Graph   (M.fromList
              [ hsdfNode "a" 3
              , hsdfNode "b" 0
              ])
              ([HSDFEdge "a" "b" 2
              , HSDFEdge "b" "a" 0
              , HSDFEdge "b" "b" 2
              ])

sdff  = Graph (M.fromList
              [ hsdfNode "S1" 0
              , hsdfNode "S2" 0
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              , hsdfNode "Z1" 0
              ])
              ([SDFEdge "S1" "f1" 0 1 1
              , SDFEdge "S1" "f2" 0 1 1
              , SDFEdge "S1" "f3" 0 1 1
              , SDFEdge "S1" "f4" 0 1 1

              , SDFEdge "S2" "f1" 0 1 1
              
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1

              , SDFEdge "f4" "Z1" 0 1 1

              , SDFEdge "Z1" "S1" 1 1 1
              , SDFEdge "Z1" "S2" 1 1 1
              ])