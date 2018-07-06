module Invariants where

import FP_Graphs
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe

type Fraction = Ratio Integer

dfsGU :: [Edge] -> [Edge]
dfsGU = undefined

repetitionVector :: [Edge] -> M.Map Label Fraction
repetitionVector
    = (foldl consistentUpdate M.empty) . dfsGU

consistentUpdate :: M.Map Label Fraction -> Edge -> M.Map Label Fraction
consistentUpdate vec edge
    | isNothing fs && isNothing fd = ((M.insert (ns edge) 1) . (M.insert (nd edge) r)) vec
    | isNothing fs                 = M.insert (ns edge) (1 / r) vec
    | isNothing fd                 = M.insert (nd edge) r vec
    | otherwise                    = if rs * r == rd then vec else error "Inconsistent graph"
    where
      fs    = M.lookup (ns edge) vec
      fd    = M.lookup (nd edge) vec
      Just rs = fs
      Just rd = fd
      r     = 1
