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


foldg1  = Graph (M.fromList
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


foldg2  = Graph (M.fromList
              [ hsdfNode "S1" 0
              , hsdfNode "S2" 0
              , hsdfNode "f'" 1
              , hsdfNode "Z1" 0
              ])
              ([SDFEdge "S1" "f'" 0 4 1

              , SDFEdge "S2" "f'" 0 4 1

              , SDFEdge "f'" "f'" 1 1 1

              , SDFEdge "f'" "Z1" 0 1 4

              , SDFEdge "Z1" "S1" 1 1 1
              , SDFEdge "Z1" "S2" 1 1 1
              ])


foldg3  = Graph (M.fromList
              [ hsdfNode "S1" 0
              , hsdfNode "S2" 0
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "Z1" 0
              ])
              ([SDFEdge "S1" "f1" 0 2 1
              , SDFEdge "S1" "f2" 0 2 1
              , SDFEdge "S2" "f1" 0 2 1

              , SDFEdge "f1" "f2" 1 1 1
              , SDFEdge "f2" "f1" 1 1 1


              , SDFEdge "f2" "Z1" 0 1 2
              , SDFEdge "Z1" "S1" 1 1 1
              , SDFEdge "Z1" "S2" 1 1 1
              ])



fg3  = Graph (M.fromList
              [ hsdfNode "S " 0
              , hsdfNode "Sf" 0
              , hsdfNode "Sg" 0
              , hsdfNode "Sh" 0
              , hsdfNode "Z " 1
              , hsdfNode "Zf" 1
              , hsdfNode "Zg" 1
              , hsdfNode "Zh" 0

              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1

              , hsdfNode "g1" 1
              , hsdfNode "g2" 1
              , hsdfNode "g3" 1
              , hsdfNode "g4" 1

              , hsdfNode "h1" 1
              , hsdfNode "h2" 1
              , hsdfNode "h3" 1
              , hsdfNode "h4" 0

              ])
              ([SDFEdge "S " "Sf" 0 1 1
              , SDFEdge "S " "Sg" 0 1 1
              , SDFEdge "S " "Sh" 0 1 1

              , SDFEdge "Sf" "f1" 0 1 1
              , SDFEdge "Sf" "f1" 0 1 1
              , SDFEdge "Sf" "f2" 0 1 1
              , SDFEdge "Sf" "f3" 0 1 1
              , SDFEdge "Sf" "f4" 0 1 1

              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1

              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "Sf" 1 1 1


              , SDFEdge "Sg" "g1" 0 1 1
              , SDFEdge "Sg" "g1" 0 1 1
              , SDFEdge "Sg" "g2" 0 1 1
              , SDFEdge "Sg" "g3" 0 1 1
              , SDFEdge "Sg" "g4" 0 1 1

              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1

              , SDFEdge "g4" "Zg" 0 1 1
              , SDFEdge "Zg" "Sg" 1 1 1


              , SDFEdge "Sh" "h1" 0 1 1
              , SDFEdge "Sh" "h1" 0 1 1
              , SDFEdge "Sh" "h2" 0 1 1
              , SDFEdge "Sh" "h3" 0 1 1
              , SDFEdge "Sh" "h4" 0 1 1

              , SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "h3" "h4" 0 1 1

              , SDFEdge "h4" "Zh" 0 1 1
              , SDFEdge "Zh" "Sh" 1 1 1

              , SDFEdge "Zf" "Z " 0 1 1
              , SDFEdge "Zg" "Z " 0 1 1
              , SDFEdge "Zh" "Z " 0 1 1
              , SDFEdge "Z " "S " 1 1 1
              ])