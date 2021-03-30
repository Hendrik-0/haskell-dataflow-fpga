module DataFlow.RepetitionVector where

import DataFlow.Types

import Graph


import qualified Data.Map as M
import Data.Maybe
import Data.List


{-
    Repetition Vector for SDF or CSDF graphs
-}
repetitionVector :: (Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l]
  -> M.Map l Integer
repetitionVector (Graph _ ns es)
  = M.map (numerator . (*l)) fs'
    where
      fs        = repetitionVectorF es
      fs'       = M.mapWithKey mulperiod fs
      denoms    = map denominator $ M.elems fs
      l         = fromIntegral $ foldl lcm 1 denoms
      --mulperiod :: Label -> Ratio Integer -> Ratio Integer
      mulperiod lbl r = fromIntegral (period rec) * r where Just rec = M.lookup lbl ns
      --mulperiod lbl r = let Just rec = M.lookup lbl ns in fromIntegral (period rec) * r

repetitionVectorF :: (Ord l, DFEdges e)
  => [e l]
  -> M.Map l Weight
repetitionVectorF
  = (foldl consistentUpdate M.empty) . dfsGU


consistentUpdate :: (Ord l, DFEdges e)
  => M.Map l Weight
  -> e l
  -> M.Map l Weight
consistentUpdate mmap edge
    | isNothing fs && isNothing fd = ((M.insert (source edge) 1) . (M.insert (target edge) r)) mmap
    | isNothing fs                 = M.insert (source edge) (rd / r) mmap
    |                 isNothing fd = M.insert (target edge) (rs * r) mmap
    | otherwise                    = if rs * r == rd then mmap else error "Inconsistent graph"
    where
      fs      = M.lookup (source edge) mmap
      fd      = M.lookup (target edge) mmap
      Just rs = fs
      Just rd = fd
      r       = sum (production edge) % sum (consumption edge)


