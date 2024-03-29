module DataFlow.StrictlyPeriodicSchedule where

import Graph
import DataFlow.Types
import DataFlow.Modulus
import DataFlow.RepetitionVector
import DataFlow.NormalizationVector
import DataFlow.MaxCycleRatio

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe


-- SPS provides maybe a M.Map with the node label as key, and a tuple with (startTime, period)
strictlyPeriodicSchedule :: (DFEdges e, Ord l, DFNodes n, Eq (e l)
  -- , Show l, Show (e l)
  )
  => Graph (M.Map l (n l)) [e l]
  -> Maybe (M.Map l (Ratio Integer, Ratio Integer))
strictlyPeriodicSchedule graph
  | isJust mcr' = Just periods
  | otherwise     = Nothing
    where
      Just mcr          = mcr'
      Just (c:_)        = mCycle
      apxGraph          = singleRateApx graph
      (mcr', mCycle)    = maxCycleRatio apxGraph
      root              = source c                                   -- take a node from the cycle
      pGraph            = edges $ df2parametricGraph apxGraph        -- convert graph to parametric graph
      Right mmap        = bellmanFord (evalEdges mcr pGraph) root  -- evaulate the graph with the MCR, bellman ford will not provide a cycle, because the nr of tokens on a the cycle is 0
      m                 = modulus graph
      q                 = repetitionVector graph
      periods           = M.intersectionWith f mmap q                -- update all periods with the repetition vector and modulus, update all start times with periods
      f (s,_) qi = (s', p)
        where
          s' = s + mcr*((m%qi) -1)
          p = mcr * (m%qi)

strictlyPeriodicScheduleWithExTime :: (DFEdges e, Ord l, DFNodes n, Eq (e l)
  -- , Show l, Show (e l)
  )
  => Graph (M.Map l (n l)) [e l]
  -> Maybe (M.Map l (Ratio Integer, Ratio Integer, Integer))
strictlyPeriodicScheduleWithExTime graph
  | isJust sps' = Just sps
  | otherwise   = Nothing
    where
      sps' = strictlyPeriodicSchedule graph
      sps = M.mapWithKey (addEx graph) (fromJust sps')
      addEx graph k (s,p) = (s,p,ex)
        where
          ex = maximum $ wcet n
          Just n = M.lookup k (nodes graph)


singleRateApx
  :: (Eq (e l), Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l]                -- DataFlowGraph
  -> Graph (M.Map l (DFNode l)) [DFEdge l]      -- HSDFGraph
singleRateApx graph@(Graph name ns es)
  = Graph name ns' es'
    where
      m     = modulus graph
      q     = repetitionVector graph
      s     = normalizationVector graph
      ns'   = M.map upperbound ns
      es'   = map (approximateEdge s) es

upperbound :: DFNodes n
  => n l        -- DFNode
  -> DFNode l   -- HSDFNode
upperbound node
  = HSDFNode (label node) (maximum (wcet node))


approximateEdge :: (DFEdges e, Eq (e l))
  => [(e l, Integer)] -- normalization vector
  -> e l              -- edge
  -> DFEdge l         -- hsdf approximation
approximateEdge s edge
  = HSDFEdge (source edge) (target edge) (numerator $ (w % 1) * toks)
    where
      Just w      = L.lookup edge s
      toks        = minimum [delta ps cs | ps <- L.inits prod, cs <- L.inits cons]
      prod        = production edge
      cons        = consumption edge
      psum        = sum prod
      csum        = sum cons
      plen        = fromIntegral $ length prod
      clen        = fromIntegral $ length cons
      g           = gcd psum csum
      delta ps cs = (g * ceiling ((tokens edge + sum ps - sum cs + 1) % g) % 1) - (i1 * psum) % plen + (j * csum) % clen
                  where
                    i1 = 1 + fromIntegral (length ps)
                    j  = fromIntegral (length cs)


printSchedule :: Show l => Maybe (M.Map l (Ratio Integer, Ratio Integer)) -> IO ()
printSchedule Nothing = putStr "N"
printSchedule (Just mmap) = putStr $ concat $ M.elems (M.mapWithKey p mmap) where
  p l (st,period) = "node: " ++ (show l) ++ ",   startTime: " ++ st' ++ ", \t period: " ++ period' ++ "\r\n" where
    st'     | denominator st == 1     = show (numerator st)
            | otherwise               = show st
    period' | denominator period == 1 = show (numerator period)
            | otherwise               = show period
