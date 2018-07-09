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

{-
    Computes the 'modulus' of the graph, which is the weighted number of tokens produced / consumed
    on any channel during a single graph iteration.
    The minimum requirement on the (unknown) edge weights is that the average production/consumption rate,
    when multiplied by the edge weight, is integral. Given the repetition vector (which gives the number of
    node firings that go in a single graph iteration), plus this requirement, we can thus calculate per
    node a number of tokens that is an integer multiple of all associated average rates.
-}
modulus :: Graph -> Integer
modulus graph
    = foldl lcm 1 (M.elems locals)
    where
      -- per node, the least common multiple of tokens produced /consumed in a single iteration,
      -- where the (weighted) production/consumption per firing is integral
      locals   = foldl f M.empty (edges graph)
      q        = repetitionVector graph
      f m edge = M.insertWith lcm v nv $ M.insertWith lcm w nw m
               where
                 (v, w)   = (ns edge, nd edge)
                 Just qv  = M.lookup v q
                 Just qw  = M.lookup w q
                 (pr, cr) = (prate edge, crate edge)
                 nv       = numerator $ fromIntegral (denominator pr * qv) * pr
                 nw       = numerator $ fromIntegral (denominator cr * qw) * cr

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

