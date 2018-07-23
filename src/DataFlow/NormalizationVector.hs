module DataFlow.NormalizationVector where

import DataFlow.Types
import DataFlow.RepetitionVector
import DataFlow.Modulus

import Graph

import Data.Map as M

{-
    Normalization Vector:
    this vector assigns every edge a weight, such that, after multiplying the production and
    consumption rate of each edge with the edge's weight, every actor produces / consumes
    the same number of tokens per firing, on average, on / from each of its outgoing / incoming edge.
-}
normalizationVector :: (Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l] 
  -> [(e l, Integer)]
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


