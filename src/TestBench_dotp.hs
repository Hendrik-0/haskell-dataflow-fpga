module TestBench_dotp where

import qualified Data.Map as M
import DataFlow
import Graph
import Hardware

import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)

g1  = Graph   (M.fromList
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

g2  = Graph   (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              , hsdfNode "f" 4
              ])
              ([SDFEdge "I" "m" 0 4 4
              , SDFEdge "m" "f" 0 4 4
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

-----------------------------------------------------------------------------------------
-- map function
-----------------------------------------------------------------------------------------

mapG = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "1" 1
              , hsdfNode "2" 1
              , hsdfNode "3" 1
              , hsdfNode "4" 1
              ])
              ([SDFEdge "I" "1" 0 1 1
              , SDFEdge "I" "2" 0 1 1
              , SDFEdge "I" "3" 0 1 1
              , SDFEdge "I" "4" 0 1 1
              , SDFEdge "1" "Z" 0 1 1
              , SDFEdge "2" "Z" 0 1 1
              , SDFEdge "3" "Z" 0 1 1
              , SDFEdge "4" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

mapGf = Graph (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              ])
              ([SDFEdge "I" "m" 0 1 1
              , SDFEdge "m" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

mapGc = Graph (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              ])
              ([SDFEdge "I" "m" 0 1 1
              , SDFEdge "m" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

-----------------------------------------------------------------------------------------
-- fold function
-----------------------------------------------------------------------------------------

foldG = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "1" 1
              , hsdfNode "2" 1
              , hsdfNode "3" 1
              , hsdfNode "4" 1
              ])
              ([SDFEdge "I" "1" 0 1 1
              , SDFEdge "I" "1" 0 1 1
              , SDFEdge "I" "2" 0 1 1
              , SDFEdge "I" "3" 0 1 1
              , SDFEdge "I" "4" 0 1 1
              , SDFEdge "1" "2" 0 1 1
              , SDFEdge "2" "3" 0 1 1
              , SDFEdge "3" "4" 0 1 1
              , SDFEdge "4" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

foldGf = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "f" 4
              ])
              ([SDFEdge "I" "f" 0 1 1
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

foldGc = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "f" 1
              ])
              ([SDFEdge "I" "f" 0 1 1
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])

-----------------------------------------------------------------------------------------
-- dotp function
-----------------------------------------------------------------------------------------

dotp = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "m1" 1
              , hsdfNode "m2" 1
              , hsdfNode "m3" 1
              , hsdfNode "m4" 1
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "I " "m1" 0 1 1
              , SDFEdge "I " "m1" 0 1 1
              , SDFEdge "I " "m2" 0 1 1
              , SDFEdge "I " "m3" 0 1 1
              , SDFEdge "I " "m4" 0 1 1
              , SDFEdge "m1" "f1" 0 1 1
              , SDFEdge "m2" "f2" 0 1 1
              , SDFEdge "m3" "f3" 0 1 1
              , SDFEdge "m4" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])


dotp' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "m1" 1
              , hsdfNode "m2" 1
              , hsdfNode "m3" 1
              , hsdfNode "m4" 1
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "I " "Im" 0 1 1

              , SDFEdge "Im" "m1" 0 1 1
              , SDFEdge "Im" "m2" 0 1 1
              , SDFEdge "Im" "m3" 0 1 1
              , SDFEdge "Im" "m4" 0 1 1
              , SDFEdge "m1" "Zm" 0 1 1
              , SDFEdge "m2" "Zm" 0 1 1
              , SDFEdge "m3" "Zm" 0 1 1
              , SDFEdge "m4" "Zm" 0 1 1

              , SDFEdge "Zm" "Im" 1 1 1
              , SDFEdge "Zm" "If" 0 1 1

              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_ms = Graph  (M.fromList
              [ hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "m1" 1
              , hsdfNode "m2" 1
              , hsdfNode "m3" 1
              , hsdfNode "m4" 1
              ])
              ([SDFEdge "Im" "m1" 0 1 1
              , SDFEdge "Im" "m2" 0 1 1
              , SDFEdge "Im" "m3" 0 1 1
              , SDFEdge "Im" "m4" 0 1 1
              , SDFEdge "m1" "Zm" 0 1 1
              , SDFEdge "m2" "Zm" 0 1 1
              , SDFEdge "m3" "Zm" 0 1 1
              , SDFEdge "m4" "Zm" 0 1 1
              , SDFEdge "Zm" "Im" 1 1 1
              ])


dotp_fs = Graph  (M.fromList
              [ hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1
              ])


dotp_m_f = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              , hsdfNode "f" 4
              ])
              ([SDFEdge "I" "m" 0 4 4
              , SDFEdge "m" "f" 0 4 4
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])


dotp_m_f' = Graph  (M.fromList
              [ hsdfNode "I" 0
              , hsdfNode "Z" 0
              , hsdfNode "m" 1
              , hsdfNode "f" 4
              ])
              ([SDFEdge "I" "m" 0 1 1
              , SDFEdge "m" "f" 0 1 1
              , SDFEdge "f" "Z" 0 1 1
              , SDFEdge "Z" "I" 1 1 1
              ])



dotp_mf = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "mf" 5
              ])
              ([SDFEdge "I " "mf" 0 4 4
              , SDFEdge "mf" "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])

dotp_mf' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "mf" 5
              ])
              ([SDFEdge "I " "mf" 0 1 1
              , SDFEdge "mf" "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])


-----------------------------------------------------------------------------------------
-- dotp function folded starting with "hieracrhy"
-----------------------------------------------------------------------------------------

dotp_ms_f = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "m1" 1
              , hsdfNode "m2" 1
              , hsdfNode "m3" 1
              , hsdfNode "m4" 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "Im" 0 4 4

              , SDFEdge "Im" "m1" 0 1 1
              , SDFEdge "Im" "m2" 0 1 1
              , SDFEdge "Im" "m3" 0 1 1
              , SDFEdge "Im" "m4" 0 1 1
              , SDFEdge "m1" "Zm" 0 1 1
              , SDFEdge "m2" "Zm" 0 1 1
              , SDFEdge "m3" "Zm" 0 1 1
              , SDFEdge "m4" "Zm" 0 1 1

              , SDFEdge "Zm" "Im" 1 1 1

              , SDFEdge "Zm" "f " 0 4 4
            --   , SDFEdge "f " "f " 1 1 1
              , SDFEdge "f " "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_ms_f' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "m1" 1
              , hsdfNode "m2" 1
              , hsdfNode "m3" 1
              , hsdfNode "m4" 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "Im" 0 1 1

              , SDFEdge "Im" "m1" 0 1 1
              , SDFEdge "Im" "m2" 0 1 1
              , SDFEdge "Im" "m3" 0 1 1
              , SDFEdge "Im" "m4" 0 1 1
              , SDFEdge "m1" "Zm" 0 1 1
              , SDFEdge "m2" "Zm" 0 1 1
              , SDFEdge "m3" "Zm" 0 1 1
              , SDFEdge "m4" "Zm" 0 1 1

              , SDFEdge "Zm" "Im" 1 1 1

              , SDFEdge "Zm" "f " 0 1 1
            --   , SDFEdge "f " "f " 1 1 1
              , SDFEdge "f " "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_mm_f = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "mm" 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "Im" 0 4 4

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1

              , SDFEdge "Zm" "f " 0 4 4
            --   , SDFEdge "f " "f " 1 1 1
              , SDFEdge "f " "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_mm_f' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "mm" 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "Im" 0 1 1

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1

              , SDFEdge "Zm" "f " 0 1 1
            --   , SDFEdge "f " "f " 1 1 1
              , SDFEdge "f " "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


-----------------------------------------------------------------------------------------
-- dotp function folded starting with everything unfolded
-----------------------------------------------------------------------------------------

dotp_mm_fs = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "mm" 1
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "I " "Im" 0 4 4

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1
              , SDFEdge "Zm" "If" 0 4 4

              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_mm_fs' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "mm" 1
              , hsdfNode "f1" 1
              , hsdfNode "f2" 1
              , hsdfNode "f3" 1
              , hsdfNode "f4" 1
              ])
              ([SDFEdge "I " "Im" 0 1 1

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1
              , SDFEdge "Zm" "If" 0 1 1

              , SDFEdge "If" "f1" 0 1 1
              , SDFEdge "If" "f2" 0 1 1
              , SDFEdge "If" "f3" 0 1 1
              , SDFEdge "If" "f4" 0 1 1
              , SDFEdge "f1" "f2" 0 1 1
              , SDFEdge "f2" "f3" 0 1 1
              , SDFEdge "f3" "f4" 0 1 1
              , SDFEdge "f4" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])
-----------------------------------------------------------------------------------------
-- dotp function folded m and f
----------------------------------------------------------------------------------------

dotp_mm_ff = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "mm" 1
              , hsdfNode "ff" 1
              ])
              ([SDFEdge "I " "Im" 0 4 4

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1
              , SDFEdge "Zm" "If" 0 4 4

              , SDFEdge "If" "ff" 0 4 1
              , SDFEdge "ff" "ff" 1 1 1
              , SDFEdge "ff" "Zf" 0 1 4

              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])


dotp_mm_ff' = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "If" 0
              , hsdfNode "Zf" 0
              , hsdfNode "mm" 1
              , hsdfNode "ff" 1
              ])
              ([SDFEdge "I " "Im" 0 1 1

              , SDFEdge "Im" "mm" 0 4 1
              , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 4

              , SDFEdge "Zm" "Im" 1 1 1
              , SDFEdge "Zm" "If" 0 1 1

              , SDFEdge "If" "ff" 0 4 1
              , SDFEdge "ff" "ff" 1 1 1
              , SDFEdge "ff" "Zf" 0 1 4

              , SDFEdge "Zf" "If" 1 1 1
              , SDFEdge "Zf" "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])

-----------------------------------------------------------------------------------------
-- dotp function folded with I and Z inside the "loop"
-----------------------------------------------------------------------------------------

dotp_miz_f = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "Im" 0
              , hsdfNode "Zm" 0
              , hsdfNode "mm" 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "Im" 0 4 1
              , SDFEdge "Im" "mm" 0 1 1
            --   , SDFEdge "mm" "mm" 1 1 1
              , SDFEdge "mm" "Zm" 0 1 1
              , SDFEdge "Zm" "Im" 1 1 1

              , SDFEdge "Zm" "f " 0 1 4
            --   , SDFEdge "f " "f " 1 1 1
              , SDFEdge "f " "Z " 0 1 1

              , SDFEdge "Z " "I " 1 1 1
              ])









dotpGf = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "m " 1
              , hsdfNode "f " 4
              ])
              ([SDFEdge "I " "m " 0 1 1
              , SDFEdge "I " "m " 0 4 4
              , SDFEdge "m " "f " 0 4 4
              , SDFEdge "f " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])

dotpGc = Graph  (M.fromList
              [ hsdfNode "I " 0
              , hsdfNode "Z " 0
              , hsdfNode "m " 1
              , hsdfNode "f " 1
              ])
              ([SDFEdge "I " "m " 0 1 1
              , SDFEdge "I " "m " 0 4 4
              , SDFEdge "m " "f " 0 4 4
              , SDFEdge "f " "Z " 0 1 1
              , SDFEdge "Z " "I " 1 1 1
              ])