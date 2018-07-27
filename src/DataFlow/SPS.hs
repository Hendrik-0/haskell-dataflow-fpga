module DataFlow.SPS where

import Graph
import DataFlow.Types
import DataFlow.Modulus
import DataFlow.RepetitionVector
import DataFlow.NormalizationVector
import DataFlow.MaxCycleRatio

import qualified Data.Map as M
import qualified Data.List as L

sps :: 
    (DFEdges e, Ord l, DFNodes n) =>
    Graph (M.Map l (n l)) [e l] ->
    M.Map l (Ratio Integer, Ratio Integer)
sps graph
    = M.map schedule mmap
    where
      (Just ratio, Just (c:cs))  = mcr graph
      root                       = source c
      pgraph                     = edges $ df2parametricGraph graph
      Right mmap                 = bellmanFord (evalEdges ratio pgraph) root
      schedule (w, _)            = (w, ratio)

singleRateApx graph@(Graph ns es)
    = Graph ns' es'
    where
      m     = modulus graph
      q     = repetitionVector graph
      s     = normalizationVector graph
      ns'   = M.map upperbound ns
      es'   = map (approximate s) es

upperbound node
    = HSDFNode (label node) (maximum (wcet node))

approximate s edge
    = HSDFEdge (source edge) (target edge) (numerator $ (w % 1) * toks)
    where
      Just w      = L.lookup edge s
      toks        = minimum [delta ps cs | ps <- L.inits (production edge), cs <- L.inits (consumption edge)]
      psum        = sum $ production edge
      csum        = sum $ consumption edge
      plen        = fromIntegral $ length $ production edge
      clen        = fromIntegral $ length $ consumption edge
      g           = gcd psum csum
      delta ps cs = (g * ceiling ((tokens edge + sum ps - sum cs + 1) % g) % 1) - (i1 * psum) % plen + (j * csum) % clen
                  where
                    i1 = 1 + fromIntegral (length ps)
                    j  = fromIntegral (length cs)
