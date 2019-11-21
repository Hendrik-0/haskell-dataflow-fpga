module TestBench_MXV where

import qualified Data.Map as M

import Graph
import DataFlow
import SVGWriter

hsdfNode l ex = (l,HSDFNode l ex)

mxv0 = Graph (M.fromList
            [("I",   HSDFNode "I"   0)
            ,("mxv", HSDFNode "mxv" 5)
            ,("Z",   HSDFNode "Z"   0)
            ])
            ([SDFEdge "I"   "mxv" 0 12 12
            , SDFEdge "I"   "mxv" 0 12 12
            , SDFEdge "mxv" "Z"   0 3  3
            , SDFEdge "Z"   "I"   1 1  1
            ])

mxv1 = Graph (M.fromList
            [("I",     HSDFNode "I"     0)
            ,("dotp0", HSDFNode "dotp0" 5)
            ,("dotp1", HSDFNode "dotp1" 5)
            ,("dotp2", HSDFNode "dotp2" 5)
            ,("Z",     HSDFNode "Z"     0)
            ])
            ([SDFEdge "I"      "dotp0" 0 4 4
            , SDFEdge "I"      "dotp0" 0 4 4
            , SDFEdge "I"      "dotp1" 0 4 4
            , SDFEdge "I"      "dotp1" 0 4 4
            , SDFEdge "I"      "dotp2" 0 4 4
            , SDFEdge "I"      "dotp2" 0 4 4
            , SDFEdge "dotp0"  "Z"     0 1 1
            , SDFEdge "dotp1"  "Z"     0 1 1
            , SDFEdge "dotp2"  "Z"     0 1 1
            , SDFEdge "Z"      "I"     1 1 1
            ])

mxv2_a = Graph (M.fromList
            [("I",   HSDFNode "I"   0)
            ,("*00", HSDFNode "*00" 1)
            ,("*01", HSDFNode "*01" 1)
            ,("*02", HSDFNode "*02" 1)
            ,("*03", HSDFNode "*03" 1)
            ,("*10", HSDFNode "*10" 1)
            ,("*11", HSDFNode "*11" 1)
            ,("*12", HSDFNode "*12" 1)
            ,("*13", HSDFNode "*13" 1)
            ,("*20", HSDFNode "*20" 1)
            ,("*21", HSDFNode "*21" 1)
            ,("*22", HSDFNode "*22" 1)
            ,("*23", HSDFNode "*23" 1)
            ,("f0",  HSDFNode "f0"  4)
            ,("f1",  HSDFNode "f1"  4)
            ,("f2",  HSDFNode "f2"  4)
            ,("Z",   HSDFNode "Z"   0)
            ])
            ([SDFEdge "I"   "*00" 0 1 1
            , SDFEdge "I"   "*00" 0 1 1
            , SDFEdge "I"   "*01" 0 1 1
            , SDFEdge "I"   "*01" 0 1 1
            , SDFEdge "I"   "*02" 0 1 1
            , SDFEdge "I"   "*02" 0 1 1
            , SDFEdge "I"   "*03" 0 1 1
            , SDFEdge "I"   "*03" 0 1 1
            , SDFEdge "I"   "*10" 0 1 1
            , SDFEdge "I"   "*10" 0 1 1
            , SDFEdge "I"   "*12" 0 1 1
            , SDFEdge "I"   "*12" 0 1 1
            , SDFEdge "I"   "*11" 0 1 1
            , SDFEdge "I"   "*11" 0 1 1
            , SDFEdge "I"   "*13" 0 1 1
            , SDFEdge "I"   "*13" 0 1 1
            , SDFEdge "I"   "*20" 0 1 1
            , SDFEdge "I"   "*20" 0 1 1
            , SDFEdge "I"   "*22" 0 1 1
            , SDFEdge "I"   "*22" 0 1 1
            , SDFEdge "I"   "*21" 0 1 1
            , SDFEdge "I"   "*21" 0 1 1
            , SDFEdge "I"   "*23" 0 1 1
            , SDFEdge "I"   "*23" 0 1 1
            , SDFEdge "*00" "f0"  0 1 1
            , SDFEdge "*01" "f0"  0 1 1
            , SDFEdge "*02" "f0"  0 1 1
            , SDFEdge "*03" "f0"  0 1 1
            , SDFEdge "*10" "f1"  0 1 1
            , SDFEdge "*11" "f1"  0 1 1
            , SDFEdge "*12" "f1"  0 1 1
            , SDFEdge "*13" "f1"  0 1 1
            , SDFEdge "*20" "f2"  0 1 1
            , SDFEdge "*21" "f2"  0 1 1
            , SDFEdge "*22" "f2"  0 1 1
            , SDFEdge "*23" "f2"  0 1 1
            , SDFEdge "f0"  "Z"   0 1 1
            , SDFEdge "f1"  "Z"   0 1 1
            , SDFEdge "f2"  "Z"   0 1 1
            , SDFEdge "Z"   "I"   1 1 1
            ])

mxv2_b = Graph (M.fromList
            [("I",   HSDFNode "I"   0)
            ,("*0", HSDFNode "*0" 1)
            ,("*1", HSDFNode "*1" 1)
            ,("*2", HSDFNode "*2" 1)
            ,("+00", HSDFNode "+00" 1)
            ,("+01", HSDFNode "+01" 1)
            ,("+02", HSDFNode "+02" 1)
            ,("+03", HSDFNode "+03" 1)
            ,("+10", HSDFNode "+10" 1)
            ,("+11", HSDFNode "+11" 1)
            ,("+12", HSDFNode "+12" 1)
            ,("+13", HSDFNode "+13" 1)
            ,("+20", HSDFNode "+20" 1)
            ,("+21", HSDFNode "+21" 1)
            ,("+22", HSDFNode "+22" 1)
            ,("+23", HSDFNode "+23" 1)
            ,("Z",   HSDFNode "Z"   0)
            ])
            ([SDFEdge "I"   "*0"  0 1 1
            , SDFEdge "I"   "*0"  0 1 1
            , SDFEdge "I"   "*1"  0 1 1
            , SDFEdge "I"   "*1"  0 1 1
            , SDFEdge "I"   "*2"  0 1 1
            , SDFEdge "I"   "*2"  0 1 1

            , SDFEdge "*0"  "+00" 0 1 1
            , SDFEdge "*0"  "+01" 0 1 1
            , SDFEdge "*0"  "+02" 0 1 1
            , SDFEdge "*0"  "+03" 0 1 1
            , SDFEdge "*1"  "+10" 0 1 1
            , SDFEdge "*1"  "+11" 0 1 1
            , SDFEdge "*1"  "+12" 0 1 1
            , SDFEdge "*1"  "+13" 0 1 1
            , SDFEdge "*2"  "+20" 0 1 1
            , SDFEdge "*2"  "+21" 0 1 1
            , SDFEdge "*2"  "+22" 0 1 1
            , SDFEdge "*2"  "+23" 0 1 1

            , SDFEdge "+00" "+01" 0 1 1
            , SDFEdge "+01" "+02" 0 1 1
            , SDFEdge "+02" "+03" 0 1 1
            , SDFEdge "+03"  "Z"  0 1 1

            , SDFEdge "+10" "+11" 0 1 1
            , SDFEdge "+11" "+12" 0 1 1
            , SDFEdge "+12" "+13" 0 1 1
            , SDFEdge "+13"  "Z"  0 1 1

            , SDFEdge "+20" "+21" 0 1 1
            , SDFEdge "+21" "+22" 0 1 1
            , SDFEdge "+22" "+23" 0 1 1
            , SDFEdge "+23" "Z"   0 1 1

            , SDFEdge "Z"   "I"   1 1 1
            ])


mxv3 = Graph (M.fromList
            [("I",   HSDFNode "I"   0)
            ,("*00", HSDFNode "*00" 1)
            ,("*01", HSDFNode "*01" 1)
            ,("*02", HSDFNode "*02" 1)
            ,("*03", HSDFNode "*03" 1)
            ,("*10", HSDFNode "*10" 1)
            ,("*11", HSDFNode "*11" 1)
            ,("*12", HSDFNode "*12" 1)
            ,("*13", HSDFNode "*13" 1)
            ,("*20", HSDFNode "*20" 1)
            ,("*21", HSDFNode "*21" 1)
            ,("*22", HSDFNode "*22" 1)
            ,("*23", HSDFNode "*23" 1)
            ,("+00", HSDFNode "+00" 1)
            ,("+01", HSDFNode "+01" 1)
            ,("+02", HSDFNode "+02" 1)
            ,("+03", HSDFNode "+03" 1)
            ,("+10", HSDFNode "+10" 1)
            ,("+11", HSDFNode "+11" 1)
            ,("+12", HSDFNode "+12" 1)
            ,("+13", HSDFNode "+13" 1)
            ,("+20", HSDFNode "+20" 1)
            ,("+21", HSDFNode "+21" 1)
            ,("+22", HSDFNode "+22" 1)
            ,("+23", HSDFNode "+23" 1)
            ,("Z",   HSDFNode "Z"   0)
            ])
            ([SDFEdge "I"   "*00" 0 1 1
            , SDFEdge "I"   "*00" 0 1 1
            , SDFEdge "I"   "*01" 0 1 1
            , SDFEdge "I"   "*01" 0 1 1
            , SDFEdge "I"   "*02" 0 1 1
            , SDFEdge "I"   "*02" 0 1 1
            , SDFEdge "I"   "*03" 0 1 1
            , SDFEdge "I"   "*03" 0 1 1
            , SDFEdge "I"   "*10" 0 1 1
            , SDFEdge "I"   "*10" 0 1 1
            , SDFEdge "I"   "*12" 0 1 1
            , SDFEdge "I"   "*12" 0 1 1
            , SDFEdge "I"   "*11" 0 1 1
            , SDFEdge "I"   "*11" 0 1 1
            , SDFEdge "I"   "*13" 0 1 1
            , SDFEdge "I"   "*13" 0 1 1
            , SDFEdge "I"   "*20" 0 1 1
            , SDFEdge "I"   "*20" 0 1 1
            , SDFEdge "I"   "*22" 0 1 1
            , SDFEdge "I"   "*22" 0 1 1
            , SDFEdge "I"   "*21" 0 1 1
            , SDFEdge "I"   "*21" 0 1 1
            , SDFEdge "I"   "*23" 0 1 1
            , SDFEdge "I"   "*23" 0 1 1

            , SDFEdge "*00" "+00" 0 1 1
            , SDFEdge "*01" "+01" 0 1 1
            , SDFEdge "*02" "+02" 0 1 1
            , SDFEdge "*03" "+03" 0 1 1
            , SDFEdge "*10" "+10" 0 1 1
            , SDFEdge "*11" "+11" 0 1 1
            , SDFEdge "*12" "+12" 0 1 1
            , SDFEdge "*13" "+13" 0 1 1
            , SDFEdge "*20" "+20" 0 1 1
            , SDFEdge "*21" "+21" 0 1 1
            , SDFEdge "*22" "+22" 0 1 1
            , SDFEdge "*23" "+23" 0 1 1

            , SDFEdge "+00" "+01" 0 1 1
            , SDFEdge "+01" "+02" 0 1 1
            , SDFEdge "+02" "+03" 0 1 1
            , SDFEdge "+03"  "Z"  0 1 1

            , SDFEdge "+10" "+11" 0 1 1
            , SDFEdge "+11" "+12" 0 1 1
            , SDFEdge "+12" "+13" 0 1 1
            , SDFEdge "+13"  "Z"  0 1 1

            , SDFEdge "+20" "+21" 0 1 1
            , SDFEdge "+21" "+22" 0 1 1
            , SDFEdge "+22" "+23" 0 1 1
            , SDFEdge "+23" "Z"   0 1 1

            , SDFEdge "Z"   "I"   1 1 1
            ])




mxv_d_0 = Graph (M.fromList
            [("I",    HSDFNode "I"    0)
            ,("dotp", HSDFNode "dotp" 5)
            ,("Z",    HSDFNode "Z"    0)
            ])
            ([SDFEdge "I"    "dotp" 0 12 4
            , SDFEdge "I"    "dotp" 0 12 4
            , SDFEdge "dotp" "Z"    0 1  3
            , SDFEdge "Z"    "I"    1 1  1
            ])

mxv_d_1a = Graph (M.fromList
            [("I",  HSDFNode "I"  0)
            ,("*0", HSDFNode "*0" 1)
            ,("*1", HSDFNode "*1" 1)
            ,("*2", HSDFNode "*2" 1)
            ,("*3", HSDFNode "*3" 1)
            ,("f",  HSDFNode "f"  4)
            ,("Z",  HSDFNode "Z"  0)
            ])
            ([SDFEdge "I"  "*0" 0 3 1
            , SDFEdge "I"  "*0" 0 3 1
            , SDFEdge "I"  "*1" 0 3 1
            , SDFEdge "I"  "*1" 0 3 1
            , SDFEdge "I"  "*2" 0 3 1
            , SDFEdge "I"  "*2" 0 3 1
            , SDFEdge "I"  "*3" 0 3 1
            , SDFEdge "I"  "*3" 0 3 1

            , SDFEdge "*0" "f"  0 1 1
            , SDFEdge "*1" "f"  0 1 1
            , SDFEdge "*2" "f"  0 1 1
            , SDFEdge "*3" "f"  0 1 1

            , SDFEdge "f"  "Z"  0 1 3
            , SDFEdge "Z"  "I"  1 1 1
            ])

mxv_d_1b = Graph (M.fromList
            [("I",  HSDFNode "I"  0)
            ,("*",  HSDFNode "*"  1)
            ,("+0", HSDFNode "+0" 1)
            ,("+1", HSDFNode "+1" 1)
            ,("+2", HSDFNode "+2" 1)
            ,("+3", HSDFNode "+3" 1)
            ,("Z",  HSDFNode "Z"  0)
            ])
            ([SDFEdge "I"  "*"  0 12 4
            , SDFEdge "I"  "*"  0 12 4

            , SDFEdge "*"  "+0" 0 1 1
            , SDFEdge "*"  "+1" 0 1 1
            , SDFEdge "*"  "+2" 0 1 1
            , SDFEdge "*"  "+3" 0 1 1

            , SDFEdge "+0" "+1" 0 1 1
            , SDFEdge "+1" "+2" 0 1 1
            , SDFEdge "+2" "+3" 0 1 1
            , SDFEdge "+3" "Z"  0 1 3

            , SDFEdge "Z"  "I"  1 1 1
            ])

mxv_d_2 = Graph (M.fromList
            [("I",  HSDFNode "I"  0)
            ,("*0", HSDFNode "*0" 1)
            ,("*1", HSDFNode "*1" 1)
            ,("*2", HSDFNode "*2" 1)
            ,("*3", HSDFNode "*3" 1)
            ,("+0", HSDFNode "+0" 1)
            ,("+1", HSDFNode "+1" 1)
            ,("+2", HSDFNode "+2" 1)
            ,("+3", HSDFNode "+3" 1)
            ,("Z",  HSDFNode "Z"  0)
            ])
            ([SDFEdge "I"  "*0" 0 3 1
            , SDFEdge "I"  "*0" 0 3 1
            , SDFEdge "I"  "*1" 0 3 1
            , SDFEdge "I"  "*1" 0 3 1
            , SDFEdge "I"  "*2" 0 3 1
            , SDFEdge "I"  "*2" 0 3 1
            , SDFEdge "I"  "*3" 0 3 1
            , SDFEdge "I"  "*3" 0 3 1

            , SDFEdge "*0" "+0" 0 1 1
            , SDFEdge "*1" "+1" 0 1 1
            , SDFEdge "*2" "+2" 0 1 1
            , SDFEdge "*3" "+3" 0 1 1

            , SDFEdge "+0" "+1" 0 1 1
            , SDFEdge "+1" "+2" 0 1 1
            , SDFEdge "+2" "+3" 0 1 1
            , SDFEdge "+3" "Z"  0 1 3

            , SDFEdge "Z"  "I"  1 1 1
            ])

mxv_dm_0 = Graph (M.fromList
            [("I",  HSDFNode "I"  0)
            ,("*",  HSDFNode "*"  1)
            ,("f",  HSDFNode "f"  4)
            ,("Z",  HSDFNode "Z"  0)
            ])
            ([SDFEdge "I" "*" 0 12 1
            , SDFEdge "I" "*" 0 12 1

            , SDFEdge "*" "f" 0 1 4

            , SDFEdge "f" "Z" 0 1 3

            , SDFEdge "Z" "I" 1 1 1
            ])

-----------------------------------------------------------------------------------------
-- steps of mxv
-----------------------------------------------------------------------------------------

sg1 = Graph (M.fromList
            [ hsdfNode "I  "  0
            , hsdfNode "*11"  1
            , hsdfNode "*12"  1
            , hsdfNode "*13"  1
            , hsdfNode "*14"  1
            , hsdfNode "+11"  1
            , hsdfNode "+12"  1
            , hsdfNode "+13"  1
            , hsdfNode "+14"  1

            , hsdfNode "*21"  1
            , hsdfNode "*22"  1
            , hsdfNode "*23"  1
            , hsdfNode "*24"  1
            , hsdfNode "+21"  1
            , hsdfNode "+22"  1
            , hsdfNode "+23"  1
            , hsdfNode "+24"  1

            , hsdfNode "*31"  1
            , hsdfNode "*32"  1
            , hsdfNode "*33"  1
            , hsdfNode "*34"  1
            , hsdfNode "+31"  1
            , hsdfNode "+32"  1
            , hsdfNode "+33"  1
            , hsdfNode "+34"  1
            , hsdfNode "Z  "  0
            ])
            [ SDFEdge "I  " "*11" 0 1 1
            , SDFEdge "I  " "*12" 0 1 1
            , SDFEdge "I  " "*13" 0 1 1
            , SDFEdge "I  " "*14" 0 1 1
            , SDFEdge "*11" "+11" 0 1 1
            , SDFEdge "*12" "+12" 0 1 1
            , SDFEdge "*13" "+13" 0 1 1
            , SDFEdge "*14" "+14" 0 1 1
            , SDFEdge "+11" "+12" 0 1 1
            , SDFEdge "+12" "+13" 0 1 1
            , SDFEdge "+13" "+14" 0 1 1
            , SDFEdge "+14" "Z  " 0 1 1

            , SDFEdge "I  " "*21" 0 1 1
            , SDFEdge "I  " "*22" 0 1 1
            , SDFEdge "I  " "*23" 0 1 1
            , SDFEdge "I  " "*24" 0 1 1
            , SDFEdge "*21" "+21" 0 1 1
            , SDFEdge "*22" "+22" 0 1 1
            , SDFEdge "*23" "+23" 0 1 1
            , SDFEdge "*24" "+24" 0 1 1
            , SDFEdge "+21" "+22" 0 1 1
            , SDFEdge "+22" "+23" 0 1 1
            , SDFEdge "+23" "+24" 0 1 1
            , SDFEdge "+24" "Z  " 0 1 1

            , SDFEdge "I  " "*31" 0 1 1
            , SDFEdge "I  " "*32" 0 1 1
            , SDFEdge "I  " "*33" 0 1 1
            , SDFEdge "I  " "*34" 0 1 1
            , SDFEdge "*31" "+31" 0 1 1
            , SDFEdge "*32" "+32" 0 1 1
            , SDFEdge "*33" "+33" 0 1 1
            , SDFEdge "*34" "+34" 0 1 1
            , SDFEdge "+31" "+32" 0 1 1
            , SDFEdge "+32" "+33" 0 1 1
            , SDFEdge "+33" "+34" 0 1 1
            , SDFEdge "+34" "Z  " 0 1 1

            , SDFEdge "Z  " "I  " 1 1 1
            ]

sg1' = Graph (M.fromList
            [ hsdfNode "I"      0
            , hsdfNode "dotp1"  5
            , hsdfNode "dotp2"  5
            , hsdfNode "dotp3"  5
            , hsdfNode "Z"      0
            ])
            [ SDFEdge "I" "dotp1" 0 1 1
            , SDFEdge "I" "dotp2" 0 1 1
            , SDFEdge "I" "dotp3" 0 1 1
            , SDFEdge "dotp1" "Z" 0 1 1
            , SDFEdge "dotp2" "Z" 0 1 1
            , SDFEdge "dotp3" "Z" 0 1 1
            , SDFEdge "Z" "I"     1 1 1
            ]



sg2 = Graph (M.fromList
            [ hsdfNode "I " 0
            , hsdfNode "*1" 1
            , hsdfNode "*2" 1
            , hsdfNode "*3" 1
            , hsdfNode "*4" 1
            , hsdfNode "+1" 1
            , hsdfNode "+2" 1
            , hsdfNode "+3" 1
            , hsdfNode "+4" 1
            , hsdfNode "Z " 0
            ])
            [ SDFEdge "I " "*1" 0 3 1
            , SDFEdge "I " "*2" 0 3 1
            , SDFEdge "I " "*3" 0 3 1
            , SDFEdge "I " "*4" 0 3 1
            , SDFEdge "*1" "+1" 0 1 1
            , SDFEdge "*2" "+2" 0 1 1
            , SDFEdge "*3" "+3" 0 1 1
            , SDFEdge "*4" "+4" 0 1 1
            , SDFEdge "+1" "+2" 0 1 1
            , SDFEdge "+2" "+3" 0 1 1
            , SDFEdge "+3" "+4" 0 1 1
            , SDFEdge "+4" "Z " 0 1 3
            , SDFEdge "Z " "I " 1 1 1
            ]



sg2' = Graph (M.fromList
            [ hsdfNode "I " 0
            , hsdfNode "If" 0
            , hsdfNode "*1" 1
            , hsdfNode "*2" 1
            , hsdfNode "*3" 1
            , hsdfNode "*4" 1
            , hsdfNode "+1" 1
            , hsdfNode "+2" 1
            , hsdfNode "+3" 1
            , hsdfNode "+4" 1
            , hsdfNode "Zf" 0
            , hsdfNode "Z " 0
            ])
            [ SDFEdge "I " "If" 0 3 1
            , SDFEdge "If" "*1" 0 1 1
            , SDFEdge "If" "*2" 0 1 1
            , SDFEdge "If" "*3" 0 1 1
            , SDFEdge "If" "*4" 0 1 1
            , SDFEdge "*1" "+1" 0 1 1
            , SDFEdge "*2" "+2" 0 1 1
            , SDFEdge "*3" "+3" 0 1 1
            , SDFEdge "*4" "+4" 0 1 1
            , SDFEdge "+1" "+2" 0 1 1
            , SDFEdge "+2" "+3" 0 1 1
            , SDFEdge "+3" "+4" 0 1 1
            , SDFEdge "+4" "Zf" 0 1 1
            , SDFEdge "Zf" "If" 1 1 1
            , SDFEdge "Zf" "Z " 0 1 3
            , SDFEdge "Z " "I " 1 1 1
            ]