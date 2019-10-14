module TestBench where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple

import DataFlow
import Graph
import Hardware

import TestBench_MXV

import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)
csdfNode l ex = (l,CSDFNode l ex)

cwgraph = Graph (M.fromList
              [("a", Node "a")
              ,("b", Node "b")
              ])
              ([WeightedEdge "a" "b" (-1)
              , WeightedEdge "b" "a" (-1)
              , WeightedEdge "a" "a" (-1)
              ])

wgraph = Graph (M.fromList
              [("S", Node "S")
              ,("A", Node "A")
              ,("B", Node "B")
              ,("C", Node "C")
              ,("D", Node "D")
              ,("E", Node "E")
              ])
              ([WeightedEdge "S" "A" 10
              , WeightedEdge "S" "E" 8
              , WeightedEdge "E" "D" 1
              , WeightedEdge "D" "A" (-4)
              , WeightedEdge "D" "C" (-1)
              , WeightedEdge "A" "C" 2
              , WeightedEdge "C" "B" (-2)
              , WeightedEdge "B" "A" 1
              ])

-- example graphs:
sdfp  = Graph (M.fromList 
              [ hsdfNode "I"   0
              , hsdfNode "mxv" 5
              , hsdfNode "Z"   0
              ])
              ([SDFEdge "I"   "mxv" 0 12 12
              , SDFEdge "I"   "mxv" 0 4 4
              , SDFEdge "mxv" "Z"   0 3 3
              , SDFEdge "Z"   "I"   1 1  1
              ])

sdff  = Graph (M.fromList 
              [ hsdfNode "I" 0
              , hsdfNode "*" 1
              , hsdfNode "f" 4
              , hsdfNode "Z" 0
              ])
              ([SDFEdge "I" "*" 0 12 1
              , SDFEdge "I" "*" 0 12 1
              , SDFEdge "*" "f" 0 1  4
              , SDFEdge "f" "Z" 0 1  3
              , SDFEdge "Z" "I" 1 1  1
              ])
             
csdf = Graph (M.fromList                      -- modulus = 36
              [ csdfNode "a" [2,3]         -- q = 4
              , csdfNode "b" [2,2]         -- q = 6
              , csdfNode "c" [3,4]         -- q = 6
              ])
              ([CSDFEdge "a" "a" 1 [1,1] [1,1]    -- weight = 9
              , CSDFEdge "a" "b" 0 [1,2] [1,1]    -- weight = 6
              , CSDFEdge "b" "a" 2 [2,0] [2,1]    -- weight = 6
              , CSDFEdge "b" "c" 0 [1,2] [2,1]    -- weight = 4
              , CSDFEdge "c" "b" 3 [2,1] [1,2]    -- weight = 4
              ])

hsdf1 = Graph (M.fromList
              [hsdfNode "a" 1
              ,hsdfNode "b" 1
              ,hsdfNode "c" 1
              ,hsdfNode "d" 1
              ,hsdfNode "e" 1
              ,hsdfNode "f" 1
              ])
              ([HSDFEdge "a" "b" 0
              , HSDFEdge "a" "c" 0
              , HSDFEdge "a" "d" 0
              , HSDFEdge "b" "e" 0
              , HSDFEdge "d" "e" 0
              , HSDFEdge "d" "f" 0
              , HSDFEdge "e" "f" 0
              , HSDFEdge "f" "b" 0
              , HSDFEdge "e" "c" 0
              , HSDFEdge "b" "a" 0
              ])




hsdf2 = Graph (M.fromList
              [hsdfNode "b" 3
              ,hsdfNode "d" 9
              ,hsdfNode "a" 1
              ,hsdfNode "c" 5
              ])
              ([HSDFEdge "b" "d" 1
              , HSDFEdge "b" "c" 1
              , HSDFEdge "b" "a" 0
              , HSDFEdge "a" "a" 1
              , HSDFEdge "a" "b" 1
              , HSDFEdge "a" "c" 1
              , HSDFEdge "d" "c" 1
              , HSDFEdge "c" "a" 1
              ])

hsdf3 = Graph (M.fromList
              [hsdfNode "a" 15
              ,hsdfNode "b" 2
              ,hsdfNode "c" 1
              ,hsdfNode "d" 1
              ,hsdfNode "e" 1
              ,hsdfNode "f" 1
              ])
              ([HSDFEdge "a" "b" 0
              , HSDFEdge "a" "c" 0
              , HSDFEdge "a" "d" 0
              , HSDFEdge "a" "a" 1
              , HSDFEdge "b" "e" 0
              , HSDFEdge "d" "e" 0
              , HSDFEdge "d" "f" 0
              , HSDFEdge "e" "f" 0
              , HSDFEdge "f" "b" 1
              , HSDFEdge "e" "c" 0
              --, HSDFEdge "b" "a" 0
              ])

hsdf4 = Graph (M.fromList
              [hsdfNode "a" 4
              ,hsdfNode "b" 3
              ,hsdfNode "c" 5
              ])
              ([HSDFEdge "a" "a" 1
              , HSDFEdge "a" "b" 0
              , HSDFEdge "b" "a" 1
              , HSDFEdge "b" "c" 0
              , HSDFEdge "c" "c" 1
              , HSDFEdge "c" "b" 0
              ])

hsdf5 = Graph (M.fromList
              [hsdfNode "a" 1
              ,hsdfNode "b" 1
              ,hsdfNode "c" 2
              ,hsdfNode "d" 1
              ])
              ([HSDFEdge "a" "b" 1
              , HSDFEdge "a" "c" 1
              , HSDFEdge "b" "d" 0
              , HSDFEdge "c" "d" 0
              , HSDFEdge "d" "a" 0
              ])


csdf2 = Graph (M.fromList
              [ csdfNode "a" [2,2]
              ])
              ([CSDFEdge "a" "a" 1 [2,1] [1,2]
              ])



bgraph = Graph (M.fromList
              [hsdfNode "a" 1
              ,hsdfNode "b" 1
              ,hsdfNode "c" 0
              ,hsdfNode "d" 0
              ,hsdfNode "e" 0
              ])
              ([SDFEdge "a" "b" 0 8 1
              , SDFEdge "b" "c" 0 1 1
              , SDFEdge "b" "e" 0 1 8
              , SDFEdge "c" "b" 1 1 1
              , SDFEdge "c" "d" 0 1 8
              , SDFEdge "d" "a" 1 1 1
              , SDFEdge "e" "c" 7 8 1
              ])

sgraph = Graph (M.fromList
              [hsdfNode "ii" 0
              ,hsdfNode "hi" 3
              ,hsdfNode "zi" 0
              ])
              ([SDFEdge "ii" "hi" 0 8 1
              , SDFEdge "hi" "zi" 0 1 8
              , SDFEdge "zi" "ii" 1 1 1
              ])

fgraph = Graph (M.fromList
              [hsdfNode "ii" 0
              ,hsdfNode "hi" 3
              ,hsdfNode "zi" 0

              ,hsdfNode "if" 0

              ,hsdfNode "+0" 1
              ,hsdfNode "+1" 1
              ,hsdfNode "+2" 1
              ,hsdfNode "+3" 1

              ,hsdfNode "iz" 0
              ,hsdfNode "gz" 2
              ,hsdfNode "zz" 0

              ,hsdfNode "zf" 0
              ])
              ([SDFEdge "ii" "hi" 0 8 1
              , SDFEdge "hi" "zi" 0 1 8
              , SDFEdge "zi" "ii" 1 1 1

              , SDFEdge "if" "+0" 0 1 1
              , SDFEdge "if" "+1" 0 1 1
              , SDFEdge "if" "+2" 0 1 1
              , SDFEdge "if" "+3" 0 1 1
              , SDFEdge "if" "ii" 0 1 1

              , SDFEdge "zi" "+0" 0 1 1
              , SDFEdge "+0" "+1" 0 1 1
              , SDFEdge "+1" "+2" 0 1 1
              , SDFEdge "+2" "+3" 0 1 1
              , SDFEdge "+3" "iz" 0 1 1

              , SDFEdge "iz" "gz" 0 8 1
              , SDFEdge "gz" "zz" 0 1 8
              , SDFEdge "zz" "iz" 1 1 1

              , SDFEdge "zz" "zf" 1 1 1

              , SDFEdge "zf" "if" 1 1 1
              ])

b0 = Graph (M.fromList
              [hsdfNode "Ia" 0
              ,hsdfNode "aa" 3
              ,hsdfNode "Za" 0

              ,hsdfNode "If" 0
              ,hsdfNode "ff" 4
              ,hsdfNode "Zf" 0

              ,hsdfNode "Ic" 0
              ,hsdfNode "cc" 2
              ,hsdfNode "Zc" 0
              ])
              ([SDFEdge "Ia" "aa" 0 8 1
              , SDFEdge "aa" "Za" 0 1 8
              , SDFEdge "Za" "Ia" 1 1 1

              , SDFEdge "Za" "If" 0 1 1

              , SDFEdge "If" "ff" 0 1 1
              , SDFEdge "ff" "Zf" 0 1 1
              , SDFEdge "Zf" "If" 1 1 1

              , SDFEdge "Zf" "Ic" 0 1 1

              , SDFEdge "Ic" "cc" 0 8 1
              , SDFEdge "cc" "Zc" 0 1 8
              , SDFEdge "Zc" "Ic" 1 1 1

              , SDFEdge "Zc" "Ia" 1 1 1              
              ])

b1 = Graph (M.fromList
              [hsdfNode "Ia" 0
              ,hsdfNode "aa" 3
              ,hsdfNode "Za" 0

              ,hsdfNode "Ic" 0
              ,hsdfNode "cc" 2
              ,hsdfNode "Zc" 0
              ])
              ([SDFEdge "Ia" "aa" 0 8 1
              , SDFEdge "aa" "Za" 0 1 8
              , SDFEdge "Za" "Ia" 1 1 1

              , SDFEdge "Za" "Ic" 0 1 1

              , SDFEdge "Ic" "cc" 0 8 1
              , SDFEdge "cc" "Zc" 0 1 8
              , SDFEdge "Zc" "Ic" 1 1 1

              , SDFEdge "Zc" "Ia" 1 1 1              
              ])

bb = Graph (M.fromList
              [hsdfNode "Ia" 0
              ,hsdfNode "aa" 3
              ,hsdfNode "Za" 0

              ,hsdfNode "If" 0
              ,hsdfNode "+0" 1
              ,hsdfNode "+1" 1
              ,hsdfNode "+2" 1
              ,hsdfNode "+3" 1
              ,hsdfNode "Zf" 0

              ,hsdfNode "Ic" 0
              ,hsdfNode "cc" 2
              ,hsdfNode "Zc" 0
              ])
              ([SDFEdge "Ia" "aa" 0 8 1
              , SDFEdge "aa" "Za" 0 1 8
              , SDFEdge "Za" "Ia" 1 1 1

              , SDFEdge "Za" "If" 0 1 1


              , SDFEdge "If" "+0" 0 1 1
              , SDFEdge "If" "+0" 0 1 1
              , SDFEdge "If" "+1" 0 1 1
              , SDFEdge "If" "+2" 0 1 1
              , SDFEdge "If" "+3" 0 1 1

              , SDFEdge "+0" "+1" 0 1 1
              , SDFEdge "+1" "+2" 0 1 1
              , SDFEdge "+2" "+3" 0 1 1
              , SDFEdge "+3" "Zf" 0 1 1

              , SDFEdge "Zf" "If" 1 1 1

              , SDFEdge "Zf" "Ic" 0 1 1

              , SDFEdge "Ic" "cc" 0 8 1
              , SDFEdge "cc" "Zc" 0 1 8
              , SDFEdge "Zc" "Ic" 1 1 1

              , SDFEdge "Zc" "Ia" 1 1 1              
              ])

b4 = Graph (M.fromList
            [hsdfNode "I"  0
            ,hsdfNode "+0" 1
            ,hsdfNode "+1" 1
            ,hsdfNode "+2" 1
            ,hsdfNode "+3" 1
            ,hsdfNode "Z"  0
            ])
            ([SDFEdge "I" "+0" 0 1 1
            , SDFEdge "I" "+0" 0 1 1
            , SDFEdge "I" "+1" 0 1 1
            , SDFEdge "I" "+2" 0 1 1
            , SDFEdge "I" "+3" 0 1 1

            , SDFEdge "+0" "+1" 0 1 1
            , SDFEdge "+1" "+2" 0 1 1
            , SDFEdge "+2" "+3" 0 1 1
            , SDFEdge "+3" "Z"  0 1 1

            , SDFEdge "Z" "I"   1 1 1
            ])

b5 = Graph (M.fromList
            [hsdfNode "Ia" 0
            ,hsdfNode "a"  1
            ,hsdfNode "Za" 1
            ,hsdfNode "Ib" 1
            ,hsdfNode "b"  1
            ,hsdfNode "Zb" 0
            ])
            ([SDFEdge "Ia" "a"  0 1 1
            , SDFEdge "a" "Za"  0 1 1
            , SDFEdge "Za" "Ia" 1 1 1

            , SDFEdge "Za" "Ib" 0 1 1

            , SDFEdge "Ib" "b"  0 1 1
            , SDFEdge "b" "+Zb" 0 1 1
            , SDFEdge "Zb" "Ib" 1 1 1
            ])
-- gvim unicode: Insert mode -> Ctrl+Shift+U
--unicode • = 2022

ro1 = Graph (M.fromList
  [hsdfNode "I"  0
  ,hsdfNode "a"  0
  ,hsdfNode "b"  0
  ,hsdfNode "c"  0
  ,hsdfNode "d"  0
  ,hsdfNode "e"  0
  ,hsdfNode "fg" 1
  ,hsdfNode "hi" 1
  ,hsdfNode "jk" 1
  ,hsdfNode "l"  1
  ,hsdfNode "o"  1
  ,hsdfNode "Z"  0
  ])
  (
  [SDFEdge "I"  "a"  0 1 1
  ,SDFEdge "I"  "b"  0 1 1
  ,SDFEdge "I"  "c"  0 1 1
  ,SDFEdge "I"  "d"  0 1 1
  ,SDFEdge "I"  "e"  0 1 1
  ,SDFEdge "a"  "fg" 0 1 1
  ,SDFEdge "b"  "fg" 0 1 1
  ,SDFEdge "c"  "fg" 0 1 1
  ,SDFEdge "d"  "fg" 0 1 1
  ,SDFEdge "fg" "hi" 0 1 1
  ,SDFEdge "e"  "hi" 0 1 1
  ,SDFEdge "hi" "jk" 0 1 1
  ,SDFEdge "hi" "l"  0 1 1
  ,SDFEdge "jk" "o"  0 1 1
  ,SDFEdge "l"  "o"  0 1 1
  ,SDFEdge "o"  "Z"  0 1 1
  ,SDFEdge "Z"  "I"  1 1 1
  ])

ex = Graph (M.fromList
  [csdfNode "a" [5]
  ,csdfNode "b" [1]
  ])
  (
  [CSDFEdge "a" "b" 0 [5] [1]
  ,CSDFEdge "b" "a" 5 [1] [5]
  ])

exa1 = Graph (M.fromList
  [csdfNode "a" [5]
  ,csdfNode "b" [1,1,1,1,1]
  ])
  (
  [CSDFEdge "a" "b" 0 [5] [0,0,0,0,5]
  ,CSDFEdge "b" "a" 5 [0,0,0,0,5] [5]
  ])



exb1 = Graph (M.fromList
  [csdfNode "a" [5,5,5,5,5]
  ,csdfNode "b" [1]
  ])
  (
  [CSDFEdge "a" "b" 5 [0,0,0,0,5] [1]
  ,CSDFEdge "b" "a" 0 [1] [0,0,0,0,5]
  ])

exb2 = Graph (M.fromList
  [csdfNode "a" [5,5,5,5,5]
  ,csdfNode "b" [1]
  ])
  (
  [CSDFEdge "a" "b" 0 [5,0,0,0,0] [1]
  ,CSDFEdge "b" "a" 5 [1] [5,0,0,0,0]
  ])


exc1 = Graph (M.fromList
  [csdfNode "a" [5,5,5,5,5]
  ,csdfNode "b" [1,1,1,1,1]
  ])
  (
  [CSDFEdge "a" "b" 0 [0,0,0,0,5] [0,0,0,0,5]
  ,CSDFEdge "b" "a" 5 [0,0,0,0,5] [0,0,0,0,5]
  ])
