module Simulation where

import Graph
import DataFlow

import qualified Data.Map as M
import qualified Data.List as L

import Data.Maybe
import Debug.Trace
-- ratioGCD :: Integral a => Ratio a -> Ratio a -> Ratio a
-- ratioGCD x y = (gcd nx ny) % (lcm dx dy)
--   where
--     nx = numerator x
--     ny = numerator y
--     dx = denominator x
--     dy = denominator y


simulation graph nrOfTicks = L.mapAccumL updateNode (graph, simMap, simTable) updateNodeInput
  where
    ns = nodes graph
    simMap = M.map (\_ -> 0) ns
    simTable = []
    tick = foldl1 gcd $ concat $ map wcet $ M.elems ns -- smallest simulation tick
    updateNodeInput = [(n, t) | t <- [0,tick..(tick*nrOfTicks)], n <- M.elems ns]
    
    


canNodeFireCount :: (Eq l, Graphs g, DFEdges e) => l -> g n [e l] -> Int -> Integer
canNodeFireCount label graph periodCount = minimum edgeConstraints -- minimum determins how many times a node can fire
  where
    etn = edgesToNode label (edges graph)           -- edges to the current node
    edgeConstraints = map allowEdgeFire etn  -- Integers representing how many times an actor can fire accorindg to every edge

    allowEdgeFire edge = div (tokens edge) ((consumption edge)!!consIndex)  -- check if there are enough tokens on the edge, maybe actor can fire multiple times, hence the div
      where
        consIndex = mod periodCount (length $ consumption edge)         -- modulus the consumption length because every edge can have its own consumption list


updateGraphWithNodeStartFiring :: (Graphs g, Eq l) => l -> g n [DFEdge l] -> Int -> Graph n [DFEdge l]
updateGraphWithNodeStartFiring label graph periodCount = (Graph ns es')
  where
    es = edges graph
    ns = nodes graph
    etn = edgesToNode label es      -- edges to the current node
    etn' = map updateEdge etn       -- Consume tokens from edges
    es' = (es L.\\ etn) ++ etn'     -- update the edges by removing the edges to the current node and adding the new edges to the current node TODO:optimize

    updateEdge edge = consumeTokens nr edge                     -- update the edge by consuming tokens, NOTE: there is no check, so tokens can be negative
      where
        consIndex = if length (consumption edge) <= 1
                      then 0
                      else mod periodCount (length $ consumption edge) -- modulus the consumption length because every edge can have its own consumption list
        nr = (consumption edge)!!consIndex                      -- the number of tokens that have to be consumed


updateGraphWithNodeEndFiring :: (Graphs g, Eq a) => g n [DFEdge a] -> (a, Int) -> Graph n [DFEdge a]
updateGraphWithNodeEndFiring graph (label, periodCount) = (Graph ns es')
  where
    es = edges graph
    ns = nodes graph
    efn = edgesFromNode label es      -- edges from the current node
    efn' = map updateEdge efn       -- Produce tokens on edges
    es' = (es L.\\ efn) ++ efn'     -- update the edges by removing the edges from the current node and adding the new edges from the current node TODO:optimize

    updateEdge edge = produceTokens nr edge                     -- update the edge by producing tokens
      where
        prodIndex = if length (production edge) <= 1
                      then 0
                      else mod periodCount (length $ production edge) -- modulus the consumption length because every edge can have its own consumption list
        nr = (production edge)!!prodIndex                      -- the number of tokens that have to be consumed


-- simMap contains the node label and period count
-- simTable is a list of all active firings in a 4 tupple as: (label, periodCount, startTime, remainingExecutionTime)

updateNode :: (DFNodes n1, Show n2) 
  => (Graph n2 [DFEdge [Char]], M.Map [Char] Int, [([Char], Int, Integer, Integer)])
  -> (n1 [Char], Integer)
  -> ((Graph n2 [DFEdge [Char]], M.Map [Char] Int, [([Char], Int, Integer, Integer)]), [([Char], Int, Integer, Integer)])
updateNode (graph, simMap, simTable) (node, tick)
  = {- trace t $ -} case iCanNodeFireCount > 0 of                   -- has a node fired, one or more times?
      False -> ((graph' , simMap , simTable' ), [])   -- no, only provide update graph and simTable (because node firings could have ended), no starting Nodes
      True  -> ((graph'', simMap', simTable''), st)   -- yes, provide the updated version of everything
  where
    t =    "\ntick: " ++ (show tick) 
        ++ "\nnode: " ++ (label node)
        ++ "\niCanNodeFireCount: " ++ (show iCanNodeFireCount)
        ++ "\nendingFirings: " ++ (show endingFirings) 
        ++ "\nremainingFirings: " ++ (show remainingFirings)
        ++ "\ngraph    : " ++ (show graph)
        ++ "\ngraph'   : " ++ (show graph')
        ++ "\ngraph''  : " ++ (show graph'')
        ++ "\n"

    nLabel =   label node
    -- First step is to see if there are any nodes instances that are will stop firing, so produce tokens.
    (endingFirings, remainingFirings) = L.partition (\(lbl, pc, st, et) -> tick >= et) simTable -- split the simtable into instances that end their firing and instances that will continue to fire
    graph' = foldl (updateGraphWithNodeEndFiring) graph $ map (\(lbl, pc, _, _) -> (lbl,pc)) endingFirings      -- update graph with all the ending nodes, meaning produce tokens on the outgoing edges
    -- simTable' = map (\(lbl, pc, st, exTime) -> (lbl, pc, st, exTime - 1)) remainingFirings          -- update the remaining nodes in the simTable
    simTable' = remainingFirings          -- update the remaining nodes in the simTable


    -- If the current node can fire, then we need to update the graph, simMap and simTable accordingly
    Just (periodCount) = M.lookup nLabel simMap                    -- TODO: fix if node not in simMap
    iCanNodeFireCount = canNodeFireCount nLabel graph' periodCount -- the number of times a node can fire
    periodCount' = periodCount + fromIntegral iCanNodeFireCount    -- the next periodCount TODO: if HSDF or CSDF, not neccesary period is always 0

    pCounts = [periodCount..(periodCount' - 1)]                             -- create a list with the pcounts for all the fire instances that are possible 
    graph'' = foldl (updateGraphWithNodeStartFiring nLabel) graph' pCounts  -- Fire (the amount of times is defined by the length of pCounts)


    startTime = tick
    st = map (addNodeFiringToSimTable node startTime) pCounts
    simTable'' = simTable' ++ st -- add the new fire instances to the simTable TODO: check if there is no mismatch between exTime and remFireTicks-1

    simMap' = M.insert nLabel periodCount' simMap


addNodeFiringToSimTable :: DFNodes n => n l -> Integer -> Int -> (l, Int, Integer, Integer)
addNodeFiringToSimTable node startTime pc = (label node, pc, startTime, endTime)
  where
    wcetIndex = if length (wcet node) <= 1
                  then 0
                  else mod pc (length $ wcet node) -- modulus the wcet length because every node can have its own wcet list 
    exTime = (wcet node)!!wcetIndex
    endTime = startTime + exTime
