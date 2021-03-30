module TestBench where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple

import DataFlow
import Graph
import Hardware

import SVGWriter

cwgraph = Graph
  "CW Graph"
  [
    Node "a"
  , Node "b"
  ]
  [
    WeightedEdge "a" "b" (-1)
  , WeightedEdge "b" "a" (-1)
  , WeightedEdge "a" "a" (-1)
  ]

wgraph = Graph
  "W Graph"
  [ Node "S"
  , Node "A"
  , Node "B"
  , Node "C"
  , Node "D"
  , Node "E"
  ]
  [WeightedEdge "S" "A" 10
  , WeightedEdge "S" "E" 8
  , WeightedEdge "E" "D" 1
  , WeightedEdge "D" "A" (-4)
  , WeightedEdge "D" "C" (-1)
  , WeightedEdge "A" "C" 2
  , WeightedEdge "C" "B" (-2)
  , WeightedEdge "B" "A" 1
  ]

-- example graphs:
sdfp  = Graph
  "SDFP"
  [
    HSDFNode "I"   0
  , HSDFNode "mxv" 5
  , HSDFNode "Z"   0
  ]
  [
    SDFEdge "I"   "mxv" 0 12 12
  , SDFEdge "I"   "mxv" 0 4 4
  , SDFEdge "mxv" "Z"   0 3 3
  , SDFEdge "Z"   "I"   1 1  1
  ]

sdff  = Graph
  "SDFF"
  [
    HSDFNode "I" 0
  , HSDFNode "*" 1
  , HSDFNode "f" 4
  , HSDFNode "Z" 0
  ]
  [
    SDFEdge "I" "*" 0 12 1
  , SDFEdge "I" "*" 0 12 1
  , SDFEdge "*" "f" 0 1  4
  , SDFEdge "f" "Z" 0 1  3
  , SDFEdge "Z" "I" 1 1  1
  ]

csdf = Graph
  "CSDF"
  [ -- modulus = 36
    CSDFNode "a" [2,3]         -- q = 4
  , CSDFNode "b" [2,2]         -- q = 6
  , CSDFNode "c" [3,4]         -- q = 6
  ]
  [
    CSDFEdge "a" "a" 1 [1,1] [1,1]    -- weight = 9
  , CSDFEdge "a" "b" 0 [1,2] [1,1]    -- weight = 6
  , CSDFEdge "b" "a" 2 [2,0] [2,1]    -- weight = 6
  , CSDFEdge "b" "c" 0 [1,2] [2,1]    -- weight = 4
  , CSDFEdge "c" "b" 3 [2,1] [1,2]    -- weight = 4
  ]

hsdf  = Graph
  "HSDF"
  [
    HSDFNode "a" 2
  , HSDFNode "b" 1
  ]
  [
    HSDFEdge "a" "b" 0
  , HSDFEdge "b" "a" 2
  ]

hsdf0 = Graph
  "HSDF 0"
  [
    HSDFNode "a" 1
  , HSDFNode "b" 2
  ]
  [
    HSDFEdge "a" "b" 0
  , HSDFEdge "b" "a" 2
  ]

hsdf1 = Graph
  "HSDF 1"
  [
    HSDFNode "a" 1
  , HSDFNode "b" 1
  , HSDFNode "c" 1
  , HSDFNode "d" 1
  , HSDFNode "e" 1
  , HSDFNode "f" 1
  ]
  [
    HSDFEdge "a" "b" 0
  , HSDFEdge "a" "c" 0
  , HSDFEdge "a" "d" 0
  , HSDFEdge "b" "e" 0
  , HSDFEdge "d" "e" 0
  , HSDFEdge "d" "f" 0
  , HSDFEdge "e" "f" 0
  , HSDFEdge "f" "b" 0
  , HSDFEdge "e" "c" 0
  , HSDFEdge "b" "a" 0
  ]

hsdf2 = Graph
  "HSDF 2"
  [
    HSDFNode "b" 3
  , HSDFNode "d" 9
  , HSDFNode "a" 1
  , HSDFNode "c" 5
  ]
  [
    HSDFEdge "b" "d" 1
  , HSDFEdge "b" "c" 1
  , HSDFEdge "b" "a" 0
  , HSDFEdge "a" "a" 1
  , HSDFEdge "a" "b" 1
  , HSDFEdge "a" "c" 1
  , HSDFEdge "d" "c" 1
  , HSDFEdge "c" "a" 1
  ]

hsdf3 = Graph
  "HSDF 3"
  [
    HSDFNode "a" 15
  , HSDFNode "b" 2
  , HSDFNode "c" 1
  , HSDFNode "d" 1
  , HSDFNode "e" 1
  , HSDFNode "f" 1
  ]
  [HSDFEdge "a" "b" 0
  , HSDFEdge "a" "c" 0
  , HSDFEdge "a" "d" 0
  , HSDFEdge "a" "a" 1
  , HSDFEdge "b" "e" 0
  , HSDFEdge "d" "e" 0
  , HSDFEdge "d" "f" 0
  , HSDFEdge "e" "f" 0
  , HSDFEdge "f" "b" 1
  , HSDFEdge "e" "c" 0
  ]

hsdf4 = Graph
  "HSDF 4"
  [
    HSDFNode "a" 4
  , HSDFNode "b" 3
  , HSDFNode "c" 5
  ]
  [
    HSDFEdge "a" "a" 1
  , HSDFEdge "a" "b" 0
  , HSDFEdge "b" "a" 1
  , HSDFEdge "b" "c" 0
  , HSDFEdge "c" "c" 1
  , HSDFEdge "c" "b" 0
  ]

hsdf5 = Graph
  "HSDF 5"
  [
    HSDFNode "a" 1
  , HSDFNode "b" 1
  , HSDFNode "c" 2
  , HSDFNode "d" 1
  ]
  [
    HSDFEdge "a" "b" 1
  , HSDFEdge "a" "c" 1
  , HSDFEdge "b" "d" 0
  , HSDFEdge "c" "d" 0
  , HSDFEdge "d" "a" 0
  ]


csdf2 = Graph
  "CSDF 2"
  [
    CSDFNode "a" [2,2]
  ]
  [
    CSDFEdge "a" "a" 1 [2,1] [1,2]
  ]



bgraph = Graph
  "B Graph"
  [
    HSDFNode "a" 1
  , HSDFNode "b" 1
  , HSDFNode "c" 0
  , HSDFNode "d" 0
  , HSDFNode "e" 0
  ]
  [
    SDFEdge "a" "b" 0 8 1
  , SDFEdge "b" "c" 0 1 1
  , SDFEdge "b" "e" 0 1 8
  , SDFEdge "c" "b" 1 1 1
  , SDFEdge "c" "d" 0 1 8
  , SDFEdge "d" "a" 1 1 1
  , SDFEdge "e" "c" 7 8 1
  ]

sgraph = Graph
  "S Graph"
  [
    HSDFNode "ii" 0
  , HSDFNode "hi" 3
  , HSDFNode "zi" 0
  ]
  [
    SDFEdge "ii" "hi" 0 8 1
  , SDFEdge "hi" "zi" 0 1 8
  , SDFEdge "zi" "ii" 1 1 1
  ]

fgraph = Graph
  "F Graph"
  [
    HSDFNode "ii" 0
  , HSDFNode "hi" 3
  , HSDFNode "zi" 0

  , HSDFNode "if" 0

  , HSDFNode "+0" 1
  , HSDFNode "+1" 1
  , HSDFNode "+2" 1
  , HSDFNode "+3" 1

  , HSDFNode "iz" 0
  , HSDFNode "gz" 2
  , HSDFNode "zz" 0

  , HSDFNode "zf" 0
  ]
  [
    SDFEdge "ii" "hi" 0 8 1
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
  ]

b0 = Graph
  "B0"
  [
    HSDFNode "Ia" 0
  , HSDFNode "aa" 3
  , HSDFNode "Za" 0

  , HSDFNode "If" 0
  , HSDFNode "ff" 4
  , HSDFNode "Zf" 0

  , HSDFNode "Ic" 0
  , HSDFNode "cc" 2
  , HSDFNode "Zc" 0
  ]
  [
    SDFEdge "Ia" "aa" 0 8 1
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
  ]

b1 = Graph
  "B1"
  [
    HSDFNode "Ia" 0
  , HSDFNode "aa" 3
  , HSDFNode "Za" 0

  , HSDFNode "Ic" 0
  , HSDFNode "cc" 2
  , HSDFNode "Zc" 0
  ]
  [
    SDFEdge "Ia" "aa" 0 8 1
  , SDFEdge "aa" "Za" 0 1 8
  , SDFEdge "Za" "Ia" 1 1 1

  , SDFEdge "Za" "Ic" 0 1 1

  , SDFEdge "Ic" "cc" 0 8 1
  , SDFEdge "cc" "Zc" 0 1 8
  , SDFEdge "Zc" "Ic" 1 1 1

  , SDFEdge "Zc" "Ia" 1 1 1
  ]

bb = Graph
  "BB"
  [
    HSDFNode "Ia" 0
  , HSDFNode "aa" 3
  , HSDFNode "Za" 0

  , HSDFNode "If" 0
  , HSDFNode "+0" 1
  , HSDFNode "+1" 1
  , HSDFNode "+2" 1
  , HSDFNode "+3" 1
  , HSDFNode "Zf" 0

  , HSDFNode "Ic" 0
  , HSDFNode "cc" 2
  , HSDFNode "Zc" 0
  ]
  [
    SDFEdge "Ia" "aa" 0 8 1
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
  ]

b4 = Graph
  "B4"
  [
    HSDFNode "I"  0
  , HSDFNode "+0" 1
  , HSDFNode "+1" 1
  , HSDFNode "+2" 1
  , HSDFNode "+3" 1
  , HSDFNode "Z"  0
  ]
  [
    SDFEdge "I" "+0" 0 1 1
  , SDFEdge "I" "+0" 0 1 1
  , SDFEdge "I" "+1" 0 1 1
  , SDFEdge "I" "+2" 0 1 1
  , SDFEdge "I" "+3" 0 1 1

  , SDFEdge "+0" "+1" 0 1 1
  , SDFEdge "+1" "+2" 0 1 1
  , SDFEdge "+2" "+3" 0 1 1
  , SDFEdge "+3" "Z"  0 1 1

  , SDFEdge "Z" "I"   1 1 1
  ]

b5 = Graph
  "B5"
  [
    HSDFNode "Ia" 0
  , HSDFNode "a"  1
  , HSDFNode "Za" 1
  , HSDFNode "Ib" 1
  , HSDFNode "b"  1
  , HSDFNode "Zb" 0
  ]
  [
    SDFEdge "Ia" "a"  0 1 1
  , SDFEdge "a" "Za"  0 1 1
  , SDFEdge "Za" "Ia" 1 1 1

  , SDFEdge "Za" "Ib" 0 1 1

  , SDFEdge "Ib" "b"  0 1 1
  , SDFEdge "b" "+Zb" 0 1 1
  , SDFEdge "Zb" "Ib" 1 1 1
  ]
-- gvim unicode: Insert mode -> Ctrl+Shift+U
--unicode • = 2022

ro1 = Graph
  "RO1"
  [
    HSDFNode "I"  0
  , HSDFNode "a"  0
  , HSDFNode "b"  0
  , HSDFNode "c"  0
  , HSDFNode "d"  0
  , HSDFNode "e"  0
  , HSDFNode "fg" 1
  , HSDFNode "hi" 1
  , HSDFNode "jk" 1
  , HSDFNode "l"  1
  , HSDFNode "o"  1
  , HSDFNode "Z"  0
  ]
  [
     SDFEdge "I"  "a"  0 1 1
  ,  SDFEdge "I"  "b"  0 1 1
  ,  SDFEdge "I"  "c"  0 1 1
  ,  SDFEdge "I"  "d"  0 1 1
  ,  SDFEdge "I"  "e"  0 1 1
  ,  SDFEdge "a"  "fg" 0 1 1
  ,  SDFEdge "b"  "fg" 0 1 1
  ,  SDFEdge "c"  "fg" 0 1 1
  ,  SDFEdge "d"  "fg" 0 1 1
  ,  SDFEdge "fg" "hi" 0 1 1
  ,  SDFEdge "e"  "hi" 0 1 1
  ,  SDFEdge "hi" "jk" 0 1 1
  ,  SDFEdge "hi" "l"  0 1 1
  ,  SDFEdge "jk" "o"  0 1 1
  ,  SDFEdge "l"  "o"  0 1 1
  ,  SDFEdge "o"  "Z"  0 1 1
  ,  SDFEdge "Z"  "I"  1 1 1
  ]

ex = Graph
  "Example"
  [
    CSDFNode "a" [5]
  , CSDFNode "b" [1]
  ]
  [
    CSDFEdge "a" "b" 0 [5] [1]
  , CSDFEdge "b" "a" 5 [1] [5]
  ]

exa1 = Graph
  "Example A1"
  [
    CSDFNode "a" [5]
  , CSDFNode "b" [1,1,1,1,1]
  ]
  [
    CSDFEdge "a" "b" 0 [5] [0,0,0,0,5]
  , CSDFEdge "b" "a" 5 [0,0,0,0,5] [5]
  ]



exb1 = Graph
  "Example B1"
  [
    CSDFNode "a" [5,5,5,5,5]
  , CSDFNode "b" [1]
  ]
  [
    CSDFEdge "a" "b" 5 [0,0,0,0,5] [1]
  , CSDFEdge "b" "a" 0 [1] [0,0,0,0,5]
  ]

exb2 = Graph
  "Example B2"
  [
    CSDFNode "a" [5,5,5,5,5]
  , CSDFNode "b" [1]
  ]
  [
    CSDFEdge "a" "b" 0 [5,0,0,0,0] [1]
  , CSDFEdge "b" "a" 5 [1] [5,0,0,0,0]
  ]


exc1 = Graph
  "Example C1"
  [
    CSDFNode "a" [5,5,5,5,5]
  , CSDFNode "b" [1,1,1,1,1]
  ]
  [
    CSDFEdge "a" "b" 0 [0,0,0,0,5] [0,0,0,0,5]
  , CSDFEdge "b" "a" 5 [0,0,0,0,5] [0,0,0,0,5]
  ]
