module GraphDataFlowAlgorithms where

import GraphAlgorithms
import GraphTypes
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe


{-
    Repetition Vector for SDF or CSDF graphs
-}
repetitionVector :: Graph -> M.Map Label Integer
repetitionVector (Graph {edges = es, nodes = ns})
    = M.map (numerator . (*l)) fs'
    where
      fs        = repetitionVectorF es
      fs'       = M.mapWithKey mulperiod fs
      denoms    = map denominator $ M.elems fs
      l         = fromIntegral $ foldl lcm 1 denoms
      mulperiod :: Label -> Ratio Integer -> Ratio Integer
      mulperiod lbl r = fromIntegral (period rec) * r where Just rec = M.lookup lbl ns
      --mulperiod lbl r = let Just rec = M.lookup lbl ns in fromIntegral (period rec) * r

repetitionVectorF :: [Edge] -> M.Map Label (Ratio Integer)
repetitionVectorF
    = (foldl consistentUpdate M.empty) . dfsGU

consistentUpdate :: M.Map Label (Ratio Integer) -> Edge -> M.Map Label (Ratio Integer)
consistentUpdate mmap edge
    | isNothing fs && isNothing fd = ((M.insert (ns edge) 1) . (M.insert (nd edge) r)) mmap
    | isNothing fs                 = M.insert (ns edge) (rd / r) mmap
    |                 isNothing fd = M.insert (nd edge) (rs * r) mmap
    | otherwise                    = if rs * r == rd then mmap else error "Inconsistent graph"
    where
      fs      = M.lookup (ns edge) mmap
      fd      = M.lookup (nd edge) mmap
      Just rs = fs
      Just rd = fd
      r       = sum (production edge) % sum (consumption edge)


{-
    TODO: GraphModulus
-}

{-
    TODO: normalization vector
-}

