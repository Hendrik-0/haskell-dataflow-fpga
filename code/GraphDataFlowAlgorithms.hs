module GraphDataFlowAlgorithms where

import GraphAlgorithms
import GraphTypes
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe
import Data.List
import Data.Tuple

{-
    Computes the 'modulus' of the graph, which is the weighted number of tokens produced / consumed
    on any channel during a single graph iteration.
    The minimum requirement on the (unknown) edge weights is that the average production/consumption rate,
    when multiplied by the edge weight, is integral. Given the repetition vector (which gives the number of
    node firings that go in a single graph iteration), plus this requirement, we can thus calculate per
    node a number of tokens that is an integer multiple of all associated average rates.
-}

modulus :: DFGraph -> Integer
modulus graph
    = foldl lcm 1 (M.elems locals)
    where
      -- per node, the least common multiple of tokens produced /consumed in a single iteration,
      -- where the (weighted) production/consumption per firing is integral
      locals   = foldl f M.empty (edges graph)
      q        = repetitionVector graph
      f m edge = M.insertWith lcm v nv $ M.insertWith lcm w nw m
               where
                 (v, w)   = (source edge, target edge)
                 Just qv  = M.lookup v q
                 Just qw  = M.lookup w q
                 (pr, cr) = (prate edge, crate edge)
                 nv       = numerator $ fromIntegral (denominator pr * qv) * pr
                 nw       = numerator $ fromIntegral (denominator cr * qw) * cr

{-
    Repetition Vector for SDF or CSDF graphs
-}
repetitionVector :: DFGraph -> M.Map Label Integer
repetitionVector (Graph ns es)
    = M.map (numerator . (*l)) fs'
    where
      fs        = repetitionVectorF es
      fs'       = M.mapWithKey mulperiod fs
      denoms    = map denominator $ M.elems fs
      l         = fromIntegral $ foldl lcm 1 denoms
      mulperiod :: Label -> Ratio Integer -> Ratio Integer
      mulperiod lbl r = fromIntegral (period rec) * r where Just rec = M.lookup lbl ns
      --mulperiod lbl r = let Just rec = M.lookup lbl ns in fromIntegral (period rec) * r

repetitionVectorF :: [DFEdge Label] -> M.Map Label (Ratio Integer)
repetitionVectorF
    = (foldl consistentUpdate M.empty) . dfsGU


consistentUpdate :: M.Map Label (Ratio Integer) -> DFEdge Label -> M.Map Label (Ratio Integer)
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


{-
    Normalization Vector:
    this vector assigns every edge a weight, such that, after multiplying the production and
    consumption rate of each edge with the edge's weight, every actor produces / consumes
    the same number of tokens per firing, on average, on / from each of its outgoing / incoming edge.
-}
normalizationVector :: DFGraph -> [(DFEdge Label, Integer)]
normalizationVector graph
    = [(edge, weight edge) | edge <- edges graph]
    where
      q           = repetitionVector graph
      n           = modulus graph
      weight edge = numerator x
                  where
                    -- Note that we use the destination + crate of edge interchangeably
                    v       = source edge
                    Just qv = M.lookup v q
                    x       = (n % qv) / prate edge


