module Invariants where

import FP_Graphs
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe

dfsGU :: [Edge] -> [Edge]
dfsGU [] = []
dfsGU (e:es)
    = path ++ dfsGU es'
    where
      root = ns e
      (es', path) = dfsU (e:es) root

repetitionVector :: Graph -> M.Map Label Integer
repetitionVector (Graph {edges = es, nodes = ns})
    = M.map (numerator . (*l)) fs'
    where
      fs        = repetitionVectorF es
      fs'       = M.mapWithKey mulperiod fs
      denoms    = map denominator $ M.elems fs
      l         = fromIntegral $ foldl lcm 1 denoms
      mulperiod :: Label -> Ratio Integer -> Ratio Integer
      mulperiod lbl r = let Just rec = M.lookup lbl ns in fromIntegral (period rec) * r

repetitionVectorF :: [Edge] -> M.Map Label (Ratio Integer)
repetitionVectorF
    = (foldl consistentUpdate M.empty) . dfsGU

consistentUpdate :: M.Map Label (Ratio Integer) -> Edge -> M.Map Label (Ratio Integer)
consistentUpdate vec edge
    | isNothing fs && isNothing fd = ((M.insert (ns edge) 1) . (M.insert (nd edge) r)) vec
    | isNothing fs                 = M.insert (ns edge) (rd / r) vec
    | isNothing fd                 = M.insert (nd edge) (rs * r) vec
    | otherwise                    = if rs * r == rd then vec else error "Inconsistent graph"
    where
      fs    = M.lookup (ns edge) vec
      fd    = M.lookup (nd edge) vec
      Just rs = fs
      Just rd = fd
      r     = sum (production edge) % sum (consumption edge)

