module DataFlow.Modulus where

import DataFlow.Types
import DataFlow.RepetitionVector
import Graph

import qualified Data.Map as M

{-
    Computes the 'modulus' of the graph, which is the weighted number of tokens produced / consumed
    on any channel during a single graph iteration.
    The minimum requirement on the (unknown) edge weights is that the average production/consumption rate,
    when multiplied by the edge weight, is integral. Given the repetition vector (which gives the number of
    node firings that go in a single graph iteration), plus this requirement, we can thus calculate per
    node a number of tokens that is an integer multiple of all associated average rates.
-}
modulus :: (Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l]
  -> Integer
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
