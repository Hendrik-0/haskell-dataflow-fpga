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



-----------------------------------------------------------------------------------------
-- Some Test graphs
-----------------------------------------------------------------------------------------
draw a = drawSchedules "../schedules" [a]

drawtgap = drawSchedules "../schedules" [tgap, tgapih, tgaph]

drawtg' = drawSchedules "../schedules" $ map fromSDFAPtoSDF [tgap, tgapih, tgaph]

tg = fromSDFAPtoSDF tgap

tgap' = Graph
  "sdf version"
    (M.fromList
  [ hsdfNode "a" 1
  , hsdfNode "b" 1
  , hsdfNode "c" 1
  , hsdfNode "d" 1
  , hsdfNode "e" 1
  , hsdfNode "sa" 4
  , hsdfNode "sd" 4
  ])
  [ SDFEdge "a" "b" 0 4 1
  , SDFEdge "b" "c" 0 2 3
  , SDFEdge "d" "c" 0 3 1
  , SDFEdge "c" "e" 0 1 3
  , SDFEdge "sa" "a" 0 1 1 -- slow down a
  , SDFEdge "sd" "d" 0 1 1 -- slow down d
  ]


tgap = Graph
  "sdf ap version"
    (M.fromList
  [ hsdfNode "a" 1
  , hsdfNode "b" 1
  , hsdfNode "c" 1
  , hsdfNode "d" 1
  , hsdfNode "e" 1
  , hsdfNode "sa" 4
  , hsdfNode "sd" 4
  ])
  [ SDFAPEdge "a" "b" 0 [4] [1]
  , SDFAPEdge "b" "c" 0 [2] [3]
  , SDFAPEdge "d" "c" 0 [3] [1]
  , SDFAPEdge "c" "e" 0 [1] [3]
  , SDFAPEdge "sa" "a" 0 [1,0,0,0] [1] -- slow down a
  , SDFAPEdge "sd" "d" 0 [1,0,0,0] [1] -- slow down d
  ]

tgaph = Graph
  "sdf ap version hierarchy of bc"
    (M.fromList
  [ hsdfNode "a" 1
  , hsdfNode "bc" 4
  , hsdfNode "d" 1
  , hsdfNode "e" 1
  , hsdfNode "sa" 4
  , hsdfNode "sd" 4
  ])
  [ SDFAPEdge "a" "bc" 0 [4] [1,1,1,0]
  , SDFAPEdge "d" "bc" 0 [3] [0,0,1,1]
  , SDFAPEdge "bc" "e" 0 [0,0,1,1] [3]
  , SDFAPEdge "sa" "a" 0 [1,0,0,0] [1] -- slow down a
  , SDFAPEdge "sd" "d" 0 [1,0,0,0] [1] -- slow down d
  ]

tgapih = Graph
  "sdf ap version inside hierarchy of bc"
    (M.fromList
  [ hsdfNode "b" 1
  , hsdfNode "c" 1
  ])
  [ SDFAPEdge "b" "c" 0 [2] [3]
  ]



-- drawtg = drawSchedules "../schedules" [tg, tgih, tgh]



-- tg = Graph
--   "sdf version"
--     (M.fromList
--   [ hsdfNode "I" 0
--   , hsdfNode "a" 1
--   , hsdfNode "b" 1
--   , hsdfNode "c" 1
--   , hsdfNode "d" 1
--   , hsdfNode "e" 1
--   , hsdfNode "Z" 0
--   ])
--   [ SDFEdge "a" "a" 1 1 1
--   , SDFEdge "b" "b" 1 1 1
--   , SDFEdge "c" "c" 1 1 1
--   , SDFEdge "d" "d" 1 1 1
--   , SDFEdge "e" "e" 1 1 1

--   , SDFEdge "I" "a" 0 1 1
--   , SDFEdge "I" "d" 0 1 1

--   , SDFEdge "a" "b" 0 3 1
--   , SDFEdge "b" "c" 0 2 3
--   , SDFEdge "d" "c" 0 2 1
--   , SDFEdge "c" "e" 0 1 2

--   , SDFEdge "e" "Z" 0 1 1
--   , SDFEdge "Z" "I" 1 1 1
--   ]

-- tgih = Graph
--   "sdf version inside hierarchy"
--     (M.fromList
--   [ hsdfNode "I" 0
--   , hsdfNode "b" 1
--   , hsdfNode "c" 1
--   , hsdfNode "Z" 0
--   ])
--   [ SDFEdge "b" "b" 1 1 1
--   , SDFEdge "c" "c" 1 1 1
--   , SDFEdge "I" "b" 0 3 1
--   , SDFEdge "I" "c" 0 2 1
--   , SDFEdge "b" "c" 0 2 3
--   , SDFEdge "c" "Z" 0 1 2
--   , SDFEdge "Z" "I" 1 1 1
--   ]

-- tgh = Graph
--   "sdf version with hierarchy"
--     (M.fromList
--   [ hsdfNode "I" 0
--   , hsdfNode "a" 1
--   , hsdfNode "bc" 4
--   , hsdfNode "d" 1
--   , hsdfNode "e" 1
--   , hsdfNode "Z" 0
--   ])
--   [ SDFEdge "a" "a" 1 1 1
--   , SDFEdge "bc" "bc" 1 1 1
--   , SDFEdge "d" "d" 1 1 1
--   , SDFEdge "e" "e" 1 1 1

--   , SDFEdge "I" "d" 0 1 1
--   , SDFEdge "I" "a" 0 1 1

--   , SDFEdge "a" "bc" 0 3 3
--   , SDFEdge "d" "bc" 0 2 2
--   , SDFEdge "bc" "e" 0 2 1

--   , SDFEdge "e" "Z" 0 1 2
--   , SDFEdge "Z" "I" 1 1 1
--   ]


-----------------------------------------------------------------------------------------
-- Dotp graphs paper SAMOS
-----------------------------------------------------------------------------------------
drawDotpsP = drawSchedules "../schedules" [dotp_1ns, dotp_5555n, dotp_1010n,dotp_20n]

dotp_1ns = Graph
  "dotp [1,1,...,1]"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 20
  , hsdfNode "4-foldl" 20
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [20] [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [20] [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1] [1]
  ]

dotp_5555n = Graph
  "dotp [5,5,5,5]"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 4
  , hsdfNode "4-foldl" 4
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [20] [5,5,5,5]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [20] [5,5,5,5]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [5,5,5,5] [5,5,5,5]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [0,0,0,1] [1]
  ]

dotp_1010n = Graph
  "dotp [10,10]"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 2
  , hsdfNode "4-foldl" 2
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [20] [10,10]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [20] [10,10]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [10,10] [10,10]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [0,1] [1]
  ]

dotp_20n = Graph
  "dotp [20]"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 1
  , hsdfNode "4-foldl" 1
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [20] [20]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [20] [20]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [20] [20]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [1] [1]
  ]


-----------------------------------------------------------------------------------------
-- Dotp graphs
-----------------------------------------------------------------------------------------

drawDotps = drawSchedules "../schedules" [dotp_2n, dotp_2nhf, dotp_2nff]

dotp_2n = Graph
  "dotp_2n"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 1
  , hsdfNode "4-foldl" 1
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [6] [6]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [6] [6]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [6] [6]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [1] [1]
  ]

dotp_2nhf = Graph
  "dotp_2nhf"
    (M.fromList
  [ hsdfNode "1-source1" 1
  , hsdfNode "2-source2" 1
  , hsdfNode "3-zipWith" 3
  , hsdfNode "4-foldl" 1
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [2] [2,2,2]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [2] [2,2,2]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [2,2,2] [6]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [1]     [1]
  ]

dotp_2nff = Graph
  "dotp_2nff"
    (M.fromList
  [ hsdfNode "1-source1" 3
  , hsdfNode "2-source2" 3
  , hsdfNode "3-zipWith" 3
  , hsdfNode "4-foldl" 3
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "3-zipWith" 0 [2] [2,2,2]
  , SDFAPEdge "2-source2" "3-zipWith" 0 [2] [2,2,2]
  , SDFAPEdge "3-zipWith" "4-foldl"   0 [2,2,2] [2,2,2]
  , SDFAPEdge "4-foldl"   "5-sink"    0 [0,0,1] [1]
  ]

-----------------------------------------------------------------------------------------
-- Lloyds graphs
-----------------------------------------------------------------------------------------

drawLloyds = drawSchedules "../schedules" [lloyds_3n, lloyds_4n, lloyds_5n, lloyds_5nf]

lloyds_3n = Graph
  "lloyds 3n"
  (M.fromList
  [ hsdfNode "1-source1" 3
  , hsdfNode "1-source2" 3
  , hsdfNode "2-cluster" 1
  , hsdfNode "3-center" 1
  , hsdfNode "4-div" 1
  , hsdfNode "5-sink" 1
  ])
  [ SDFAPEdge "1-source1" "2-cluster" 0 [6,6,6] [18]
  , SDFAPEdge "1-source2" "2-cluster" 0 [1,1,1] [3]
  , SDFAPEdge "1-source2" "2-center"  0 [1,1,1] [3]
  , SDFAPEdge "2-cluster" "3-center"  0 [18]    [18]
  , SDFAPEdge "3-center"  "4-div"     0 [3]     [3]
  , SDFAPEdge "4-div"     "5-sink"    0 [3]     [3]
  ]


lloyds_4n = Graph
  "lloyds 4n"
  (M.fromList
  [ hsdfNode "1-source1" 3
  , hsdfNode "1-source2" 3
  , hsdfNode "2-cluster" 1
  , hsdfNode "3-csasc" 1
  , hsdfNode "4-csi" 1
  , hsdfNode "5-div" 1
  , hsdfNode "6-sink" 1
  ])
  [ SDFAPEdge "1-source1" "2-cluster" 0 [6,6,6] [18]
  , SDFAPEdge "1-source2" "2-cluster" 0 [1,1,1] [3]
  , SDFAPEdge "1-source2" "4-csi"     0 [1,1,1] [3]
  , SDFAPEdge "2-cluster" "3-csasc"   0 [18]    [18]
  , SDFAPEdge "4-csi"     "3-csasc"   0 [3]     [3]
  , SDFAPEdge "3-csasc"   "5-div"     0 [3]     [3]
  , SDFAPEdge "5-div"     "6-sink"    0 [3]     [3]
  ]

lloyds_5n = Graph
  "lloyds 5n"
  (M.fromList
  [ hsdfNode "1-source1" 3
  , hsdfNode "1-source2" 3
  , hsdfNode "2-sdist" 1
  , hsdfNode "3-mins" 1
  , hsdfNode "4-csasc" 1
  , hsdfNode "5-csi" 1
  , hsdfNode "6-div" 1
  , hsdfNode "7-sink" 1
  ])
  [ SDFAPEdge "1-source1" "2-sdist" 0 [6,6,6] [18]
  , SDFAPEdge "1-source2" "2-sdist" 0 [1,1,1] [3]
  , SDFAPEdge "1-source2" "5-csi"   0 [1,1,1] [3]
  , SDFAPEdge "2-sdist"   "3-mins"  0 [54]    [54]
  , SDFAPEdge "3-mins"    "4-csasc" 0 [18]    [18]
  , SDFAPEdge "5-csi"     "4-csasc" 0 [3]     [3]
  , SDFAPEdge "4-csasc"   "6-div"   0 [3]     [3]
  , SDFAPEdge "6-div"     "7-sink"  0 [3]     [3]
  ]

lloyds_5nf = Graph
  "lloyds 5nf"
  (M.fromList
  [ hsdfNode "1-source1" 3
  , hsdfNode "1-source2" 3
  , hsdfNode "2-sdist" 3
  , hsdfNode "3-mins" 3
  , hsdfNode "4-csasc" 3
  , hsdfNode "5-csi" 3
  , hsdfNode "6-div" 3
  , hsdfNode "7-sink" 1
  ])
  [ SDFAPEdge "1-source1" "2-sdist" 0 [6,6,6]     [6,6,6]
  , SDFAPEdge "1-source2" "2-sdist" 0 [1,1,1]     [1,1,1]
  , SDFAPEdge "1-source2" "5-csi"   0 [1,1,1]     [1,1,1]
  , SDFAPEdge "2-sdist"   "3-mins"  0 [18,18,18]  [18,18,18]
  , SDFAPEdge "3-mins"    "4-csasc" 0 [6,6,6]     [6,6,6]
  , SDFAPEdge "5-csi"     "4-csasc" 0 [1,1,1]     [1,1,1]
  , SDFAPEdge "4-csasc"   "6-div"   0 [1,1,1]     [1,1,1]
  , SDFAPEdge "6-div"     "7-sink"  0 [1,1,1]     [3]
  ]


