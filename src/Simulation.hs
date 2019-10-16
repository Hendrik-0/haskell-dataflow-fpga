module Simulation where

import Graph
import DataFlow

import qualified Data.Map as M
import qualified Data.List as L

import Data.Maybe
-- ratioGCD :: Integral a => Ratio a -> Ratio a -> Ratio a
-- ratioGCD x y = (gcd nx ny) % (lcm dx dy)
--   where
--     nx = numerator x
--     ny = numerator y
--     dx = denominator x
--     dy = denominator y


simulation graph = 1
  where
    ns = nodes graph
    simMap = M.map (\_ -> (0,[])) ns
    tick = foldl1 gcd $ concat $ map wcet $ M.elems ns -- smallest simulation tick
    es = edges graph
    -- nsCanFire = filter (\edge -> (consumption edge) <= (tokens edge)) es


-- canNodeFire :: (Graphs g, DFEdges e, Eq l) => l -> Int -> g n [e l] -> Bool
canNodeFire graph label periodCount = and sufficientTokensOnEdge
  where
    etn = edgesToNode label (edges graph)           -- edges to the current node
    sufficientTokensOnEdge = map allowEdgeFire etn  -- Booleans if an edge is keeping a node from firing, not sufficient tokens

    allowEdgeFire edge = (consumption edge)!!consIndex <= tokens edge   -- check if there are enough tokens on the edge
      where
        consIndex = mod periodCount (length $ consumption edge)         -- modulus the consumption length because every edge can have its own consumption list


-- updateGraphWithNodeStartFiring :: (Graphs g, Eq l) => l -> Int -> g n [DFEdge l] -> Graph n [DFEdge l]
updateGraphWithNodeStartFiring graph (label, periodCount) = (Graph ns es')
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


-- updateGraphWithNodeEndFiring :: (Graphs g, Eq l) => l -> Int -> g n [DFEdge l] -> Graph n [DFEdge l]
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



updateGraphAndSimTable graph simTable = (graph', simTable')
  where
    (endingFirings, remainingFirings) = L.partition (\(lbl, pc, remFireTicks) -> remFireTicks > 0) simTable -- split the simtable
    graph' = foldl updateGraphWithNodeEndFiring graph (map (\(lbl, pc,_) -> (lbl,pc)) endingFirings) -- update graph with all the ending nodes
    simTable' = map (\(lbl, pc, remFireTicks) -> (lbl, pc, remFireTicks - 1)) remainingFirings -- update the remaining nodes in the simTable


-- simMap contains the current periodCount of every node
-- simTable is a list conting all the firing instances of every node

updateNode label graph simMap
  = case canNodeFire graph label periodCount of
      True -> 1
      False -> 0
  where
    Just periodCount = lookup label simMap -- TODO: fix if node not in simMap
    bCanNodeFire = canNodeFire graph label periodCount
    simMap' = case bCanNodeFire of
                True -> M.insert        -- node can fire, so that will happen, and periodCount needs to be increased
                False -> simMap         -- can not fire




iDontKnowAFunctionName graph simTable = 1
  where
    ns = M.keys $ nodes graph -- ns are node labels
    (graph', simTable') = updateGraphAndSimTable graph simTable







-- updateSimTableAndGraph graph simTable = (graph', simTable')
--    where
--     (endingFirings, remainingFirings) = span

-- -- doSomethingWithNode label graph simMap = simMap'
-- --   where
--     Just (periodCount, simTable) = M.lookup label simMap           -- lookup node in mmap, TODO: fix if missing
--     bCanNodeFire = canNodeFire label periodCount graph
--     activeFirings = length simTable
--     (periodCount', (simTable', graph')) = case (bCanNodeFire, activeFirings) of
--       (False, 0) -> (periodCount, (simTable, graph)) -- node can not fire, there are no active firings, everything remains unchanged
--       (False, x) -> (periodCount, updateSimTableAndGraph simTable graph) -- node can not fire, there are active firings of this node, update the simTable and graph accordingly





checkAndFireNode label graph simMap
  = case (and sufficientTokensOnEdge, last sim) of -- if the last of sim is not 0, then the node is firing
      (False,0) -> (graph, simMapUN) -- node cannot fire, and not firing
      (False,1) -> (graph, simMapUF) -- node cannot fire, but its the last tick
      (False,x) -> (graph, simMapUF) -- node cannot fire, but firing
      (True,0)  -> (Graph ns es', simMapFN) -- node can fire, and is not firing
      (True,1)  -> (Graph ns es', simMapFF) -- node can fire, but it is the last tick
      (True,x)  -> (Graph ns es', simMapFF) -- node can fire, but is firing TODO: what to do in this case?
  where
    es = edges graph
    ns = nodes graph
    Just node = M.lookup label ns                             -- lookup node in graph, TODO: fix if missing
    Just (periodCount, sim) = M.lookup label simMap           -- lookup node in mmap, TODO: fix if missing

    etn = edgesToNode label es                                -- edges to the current node
    sufficientTokensOnEdge = map allowEdgeFire etn             -- Booleans if an edge is keeping a node from firing, not sufficient tokens
    etn' = map updateEdge etn                                 -- Consume tokens from edges
    es' = (es L.\\ etn) ++ etn'     -- update the edges by removing the edges to the current node and adding the new edges to the current node TODO:optimize

    wcetIndex = mod periodCount (length $ wcet node) -- modulus the wcet length because every node can have its own wcet list
    wcet' = (wcet node)!!wcetIndex

    -- insert will overwrite the old value if it exists
    simMapUN = M.insert label (periodCount, sim ++ [0]) simMap                -- cannot fire, not firing, so periodCount stays the same,
    simMapUF = M.insert label (periodCount + 1, sim ++ [(last sim)-1]) simMap   -- cannot fire, but is firing, so update periodCount stays and extend sim
    simMapFN = M.insert label (periodCount + 1, sim ++ [ wcet'])       simMap   -- can fire, not firing, so update periodCount, and extend sim with wcet
    simMapFF = M.insert label (periodCount + 1, sim ++ [ wcet'])       simMap   -- can fire, but firing, TODO: do not know what to do yet

    updateEdge edge = consumeTokens nr edge                             -- update the edge by consuming tokens, NOTE: there is no check, so tokens can be negative
      where
        consIndex = mod periodCount (length $ consumption edge)         -- modulus the consumption length because every edge can have its own consumption list
        nr = (consumption edge)!!consIndex                              -- the number of tokens that have to be consumed

    allowEdgeFire edge = (consumption edge)!!consIndex <= tokens edge   -- check if there are enough tokens on the edge
      where
        consIndex = mod periodCount (length $ consumption edge)         -- modulus the consumption length because every edge can have its own consumption list




-- nodesThatCanFire simMap graph = []
--   where

