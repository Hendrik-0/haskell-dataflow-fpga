module TestBench_fold where

import qualified Data.Map as M
import DataFlow
import Graph
import Hardware

import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)

hs  = Graph "hs"   (M.fromList
              [ hsdfNode "a" 3
              , hsdfNode "b" 0
              ])
              ([HSDFEdge "a" "b" 2
              , HSDFEdge "b" "a" 0
              , HSDFEdge "b" "b" 2
              ])


foldg1  = Graph "foldg1" (M.fromList
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


foldg2  = Graph "foldg2" (M.fromList
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


foldg3  = Graph "foldg3" (M.fromList
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



fg3  = Graph "fg3" (M.fromList
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


g1  = Graph "g1" (M.fromList
              [ hsdfNode "a" 2
              , hsdfNode "b" 1
              , hsdfNode "c" 3
              ])
              ([SDFEdge "a" "b" 0 4 1
              , SDFEdge "b" "c" 0 1 1
              , SDFEdge "c" "a" 4 1 4
              ])


step1 = Graph "step1" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              , hsdfNode "g1" 1
              , hsdfNode "g2" 1
              , hsdfNode "g3" 1
              , hsdfNode "g4" 1
              ])
              ([SDFEdge "I " "f1" 0 1 1
              , SDFEdge "I " "f2" 0 1 1
              , SDFEdge "I " "f3" 0 1 1
              , SDFEdge "I " "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Z " 0 1 1

              , SDFEdge "I " "g1" 0 1 1
              , SDFEdge "I " "g2" 0 1 1
              , SDFEdge "I " "g3" 0 1 1
              , SDFEdge "I " "g4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1
              , SDFEdge "g4" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])

step1' = Graph "step1'" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "ft" 4
              , hsdfNode "g1" 1
              , hsdfNode "g2" 1
              , hsdfNode "g3" 1
              , hsdfNode "g4" 1
              ])
              ([SDFEdge "I " "ft" 0 4 4
              , SDFEdge "ft" "Z " 0 1 1

              , SDFEdge "I " "g1" 0 1 1
              , SDFEdge "I " "g2" 0 1 1
              , SDFEdge "I " "g3" 0 1 1
              , SDFEdge "I " "g4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1
              , SDFEdge "g4" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])



step1'' = Graph "step1''" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "ft" 4
              , hsdfNode "gt" 4
              ])
              ([SDFEdge "I " "ft" 0 4 4
              , SDFEdge "ft" "Z " 0 1 1

              , SDFEdge "I " "gt" 0 4 4
              , SDFEdge "gt" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])



step2 = Graph "step2" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "If" 0
              , hsdfNode "ff" 1
              , hsdfNode "Zf" 0
              , hsdfNode "g1" 1
              , hsdfNode "g2" 1
              , hsdfNode "g3" 1
              , hsdfNode "g4" 1
              ])
              ([SDFEdge "I " "If" 0 1 1
              , SDFEdge "If" "ff" 0 4 1
              , SDFEdge "ff" "ff" 1 1 1
              , SDFEdge "ff" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "I " "g1" 0 1 1
              , SDFEdge "I " "g2" 0 1 1
              , SDFEdge "I " "g3" 0 1 1
              , SDFEdge "I " "g4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1
              , SDFEdge "g4" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


ng = Graph "ng" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0

              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "f'" 1

              , hsdfNode "Ig" 0
              , hsdfNode "Zg" 0
              , hsdfNode "g1" 1
              , hsdfNode "g2" 1

              , hsdfNode "Ih" 0
              , hsdfNode "Zh" 0
              , hsdfNode "h1" 1
              , hsdfNode "h2" 1
              , hsdfNode "h3" 1
              , hsdfNode "h4" 1
              ])
              ([SDFEdge "I " "If" 0 1 1
              , SDFEdge "I " "Ig" 0 1 1
              , SDFEdge "I " "Ih" 0 1 1

              , SDFEdge "If" "f'" 0 4 1
              , SDFEdge "f'" "f'" 1 1 1
              , SDFEdge "f'" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1

              , SDFEdge "Ig" "g1" 0 2 1
              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g1" 1 1 1
              , SDFEdge "g2" "Zg" 0 1 2
              , SDFEdge "Zg" "Ig" 1 1 1

              , SDFEdge "Ih" "h1" 0 1 1
              , SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "h4" "Zh" 0 1 1
              , SDFEdge "Zh" "Ih" 1 1 1

              , SDFEdge "Zf" "Z " 0 1 1
              , SDFEdge "Zg" "Z " 0 1 1
              , SDFEdge "Zh" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])



ng1 = Graph "ng1" (M.fromList
              [ hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "f'" 1
              ])
              ([SDFEdge "If" "f'" 0 4 1
              , SDFEdge "f'" "f'" 1 1 1
              , SDFEdge "f'" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1
              ])

ng2 = Graph "ng2" (M.fromList
              [ hsdfNode "Ig" 0
              , hsdfNode "Zg" 0
              , hsdfNode "g1" 1
              , hsdfNode "g2" 1
              ])
              ([SDFEdge "Ig" "g1" 0 2 1
              , SDFEdge "g1" "g2" 0 1 1
              , SDFEdge "g2" "g1" 1 1 1
              , SDFEdge "g2" "Zg" 0 1 2
              , SDFEdge "Zg" "Ig" 1 1 1
              ])

ng3 = Graph "ng3" (M.fromList
              [ hsdfNode "Ih" 0
              , hsdfNode "Zh" 0
              , hsdfNode "h1" 1
              , hsdfNode "h2" 1
              , hsdfNode "h3" 1
              , hsdfNode "h4" 1
              ])
              ([SDFEdge "Ih" "h1" 0 1 1
              , SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "h4" "Zh" 0 1 1
              , SDFEdge "Zh" "Ih" 1 1 1
              ])




------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

tg = Graph "tg" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0

              , hsdfNode "a " 3
              , hsdfNode "b " 2

              , hsdfNode "f1" 1,    hsdfNode "g1" 1,    hsdfNode "h1" 1
              , hsdfNode "f2" 1,    hsdfNode "g2" 1,    hsdfNode "h2" 1
              , hsdfNode "f3" 1,    hsdfNode "g3" 1,    hsdfNode "h3" 1
              , hsdfNode "f4" 1,    hsdfNode "g4" 1,    hsdfNode "h4" 1
              ])
              ([SDFEdge "I " "a " 0 1 1

              , SDFEdge "a " "f1" 0 1 1,    SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "f1" 0 1 1,    SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "f2" 0 1 1,    SDFEdge "a " "g2" 0 1 1,    SDFEdge "a " "h2" 0 1 1
              , SDFEdge "a " "f3" 0 1 1,    SDFEdge "a " "g3" 0 1 1,    SDFEdge "a " "h3" 0 1 1
              , SDFEdge "a " "f4" 0 1 1,    SDFEdge "a " "g4" 0 1 1,    SDFEdge "a " "h4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1,    SDFEdge "g1" "g2" 0 1 1,    SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1,    SDFEdge "g2" "g3" 0 1 1,    SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1,    SDFEdge "g3" "g4" 0 1 1,    SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "f4" "b " 0 1 1,    SDFEdge "g4" "b " 0 1 1,    SDFEdge "h4" "b " 0 1 1

              , SDFEdge "b " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])

tg1 = Graph "tg1" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0

              , hsdfNode "If" 0
              , hsdfNode "Zf" 0

              , hsdfNode "a " 3
              , hsdfNode "b " 2

              , hsdfNode "f1" 1,    hsdfNode "g1" 1,    hsdfNode "h1" 1
              , hsdfNode "f2" 1,    hsdfNode "g2" 1,    hsdfNode "h2" 1
              , hsdfNode "f3" 1,    hsdfNode "g3" 1,    hsdfNode "h3" 1
              , hsdfNode "f4" 1,    hsdfNode "g4" 1,    hsdfNode "h4" 1
              ])
              ([SDFEdge "I " "a " 0 1 1

              , SDFEdge "a " "If" 0 1 1
              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "b " 0 1 1

              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g2" 0 1 1,    SDFEdge "a " "h2" 0 1 1
              , SDFEdge "a " "g3" 0 1 1,    SDFEdge "a " "h3" 0 1 1
              , SDFEdge "a " "g4" 0 1 1,    SDFEdge "a " "h4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1,    SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1,    SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1,    SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "g4" "b " 0 1 1,    SDFEdge "h4" "b " 0 1 1

              , SDFEdge "b " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])


tg1' = Graph "tg1'" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0

              , hsdfNode "pf" 4

              , hsdfNode "a " 3
              , hsdfNode "b " 2

              , hsdfNode "g1" 1,    hsdfNode "h1" 1
              , hsdfNode "g2" 1,    hsdfNode "h2" 1
              , hsdfNode "g3" 1,    hsdfNode "h3" 1
              , hsdfNode "g4" 1,    hsdfNode "h4" 1
              ])
              ([SDFEdge "I " "a " 0 1 1

              , SDFEdge "a " "pf" 0 1 1
              , SDFEdge "pf" "pf" 1 1 1
              , SDFEdge "pf" "b " 0 1 1

              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g2" 0 1 1,    SDFEdge "a " "h2" 0 1 1
              , SDFEdge "a " "g3" 0 1 1,    SDFEdge "a " "h3" 0 1 1
              , SDFEdge "a " "g4" 0 1 1,    SDFEdge "a " "h4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1,    SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1,    SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1,    SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "g4" "b " 0 1 1,    SDFEdge "h4" "b " 0 1 1

              , SDFEdge "b " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])


tg2' =  Graph "tg2'" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0

              , hsdfNode "If" 0
              , hsdfNode "f'" 1
              , hsdfNode "Zf" 0

              , hsdfNode "a " 3
              , hsdfNode "b " 2

              , hsdfNode "g1" 1,    hsdfNode "h1" 1
              , hsdfNode "g2" 1,    hsdfNode "h2" 1
              , hsdfNode "g3" 1,    hsdfNode "h3" 1
              , hsdfNode "g4" 1,    hsdfNode "h4" 1
              ])
              ([SDFEdge "I " "a " 0 1 1

              , SDFEdge "a " "If" 0 1 1
              , SDFEdge "If" "f'" 0 4 1
              , SDFEdge "f'" "f'" 1 1 1
              , SDFEdge "f'" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "b " 0 1 1

              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g1" 0 1 1,    SDFEdge "a " "h1" 0 1 1
              , SDFEdge "a " "g2" 0 1 1,    SDFEdge "a " "h2" 0 1 1
              , SDFEdge "a " "g3" 0 1 1,    SDFEdge "a " "h3" 0 1 1
              , SDFEdge "a " "g4" 0 1 1,    SDFEdge "a " "h4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1,    SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1,    SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1,    SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "g4" "b " 0 1 1,    SDFEdge "h4" "b " 0 1 1

              , SDFEdge "b " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])




tg2 = Graph "tg2" (M.fromList
              [ hsdfNode "I " 0

              , hsdfNode "Ia" 0
              , hsdfNode "a " 3
              , hsdfNode "Za" 0

              , hsdfNode "If" 0
              , hsdfNode "f'" 1
              , hsdfNode "Zf" 0

              , hsdfNode "Ig" 0,    hsdfNode "Ih" 0
              , hsdfNode "g1" 1,    hsdfNode "h1" 1
              , hsdfNode "g2" 1,    hsdfNode "h2" 1
              , hsdfNode "g3" 1,    hsdfNode "h3" 1
              , hsdfNode "g4" 1,    hsdfNode "h4" 1
              , hsdfNode "Zg" 0,    hsdfNode "Zh" 0

              , hsdfNode "Ib" 0
              , hsdfNode "b " 2
              , hsdfNode "Zb" 0

              , hsdfNode "Z " 0
              ])
              ([SDFEdge "I " "Ia" 0 1 1

              , SDFEdge "Ia" "a " 0 1 1
              -- , SDFEdge "a " "a " 1 1 1
              , SDFEdge "a " "Za" 0 1 1
              , SDFEdge "Za" "Ia" 1 1 1

              , SDFEdge "Za" "If" 0 1 1
              , SDFEdge "If" "f'" 0 4 1
              , SDFEdge "f'" "f'" 1 1 1
              , SDFEdge "f'" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Ib" 0 1 1

              , SDFEdge "Za" "Ig" 0 1 1,    SDFEdge "Za" "Ih" 0 1 1
              , SDFEdge "Ig" "g1" 0 1 1,    SDFEdge "Ih" "h1" 0 1 1
              , SDFEdge "Ig" "g1" 0 1 1,    SDFEdge "Ih" "h1" 0 1 1
              , SDFEdge "Ig" "g2" 0 1 1,    SDFEdge "Ih" "h2" 0 1 1
              , SDFEdge "Ig" "g3" 0 1 1,    SDFEdge "Ih" "h3" 0 1 1
              , SDFEdge "Ig" "g4" 0 1 1,    SDFEdge "Ih" "h4" 0 1 1
              , SDFEdge "g1" "g2" 0 1 1,    SDFEdge "h1" "h2" 0 1 1
              , SDFEdge "g2" "g3" 0 1 1,    SDFEdge "h2" "h3" 0 1 1
              , SDFEdge "g3" "g4" 0 1 1,    SDFEdge "h3" "h4" 0 1 1
              , SDFEdge "g4" "Zg" 0 1 1,    SDFEdge "h4" "Zh" 0 1 1
              , SDFEdge "Zg" "Ig" 1 1 1,    SDFEdge "Zh" "Ih" 1 1 1
              , SDFEdge "Zg" "Ib" 0 1 1,    SDFEdge "Zh" "Ib" 0 1 1

              , SDFEdge "Ib" "b " 0 1 1
              -- , SDFEdge "b " "b " 1 1 1
              , SDFEdge "b " "Zb" 0 1 1
              , SDFEdge "Zb" "Ib" 1 1 1
              , SDFEdge "Zb" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


cg2 = Graph "cg2" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "a'" 1
              , hsdfNode "If" 0
              , hsdfNode "pf" 1
              , hsdfNode "Zf" 0
              , hsdfNode "g'" 1
              , hsdfNode "h'" 1
              , hsdfNode "b'" 1
              ])
              ([SDFEdge "I " "a'" 0 1 1

              , SDFEdge "a'" "If" 0 1 1
              , SDFEdge "If" "pf" 0 4 1
              , SDFEdge "pf" "pf" 1 1 1
              , SDFEdge "pf" "Zf" 0 1 4
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "b'" 0 1 1

              , SDFEdge "a'" "g'" 0 1 1
              , SDFEdge "g'" "b'" 0 1 1

              , SDFEdge "a'" "h'" 0 1 1
              , SDFEdge "h'" "b'" 0 1 1

              , SDFEdge "b'" "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])


cg2' = Graph "cg2'" (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "a'" 1
              , hsdfNode "f'" 4
              , hsdfNode "g'" 1
              , hsdfNode "h'" 1
              , hsdfNode "b'" 1
              ])
              ([SDFEdge "I " "a'" 0 1 1

              , SDFEdge "a'" "f'" 0 1 1
              , SDFEdge "f'" "b'" 0 1 1

              , SDFEdge "a'" "g'" 0 1 1
              , SDFEdge "g'" "b'" 0 1 1

              , SDFEdge "a'" "h'" 0 1 1
              , SDFEdge "h'" "b'" 0 1 1

              , SDFEdge "b'" "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------





tgf =  Graph "tgf" (M.fromList
              [ hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1

              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1

              , SDFEdge "Zf" "If" 1 1 1
              ])

s2 =  Graph "s2" (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "f" 1
              ])
              ([SDFEdge "I" "f" 0 4 1
              , SDFEdge "f" "f" 1 1 1
              , SDFEdge "f" "Z" 0 1 4
              , SDFEdge "Z" "I" 1 1 1
              ])