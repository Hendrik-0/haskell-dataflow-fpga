module DataFlow.MaxCycleRatio where

import Graph

import DataFlow.Types

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Either

import Debug.Trace

{-
    MaxCycleRatio
-}
mcr :: (Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l]
  -> (Maybe Weight, Maybe [Edge l])
mcr g
  | isLeft pmapSpTree = (ratio, Just cycle)                               -- initial spanningTree already contains a cycle
  | otherwise         = mcrR root candidates spTree
    where
      pgraph              = df2parametricGraph g                                    -- convert dataflow graph to a graph conting parametric distances
      (root:_)            = M.keys $ nodes g                                        -- pick a "random" node as root
      largeWeight         = sum [max (weight edge) 0 | edge <- (edges pgraph)] + 1  -- dit is gewoon "een groot getal" (HF)
      pmapSpTree          = pmapAndSpanningTree (edges pgraph) root largeWeight     -- create spanningTree of the parametric graph with a largeWeigth
      Left cycles         = pmapSpTree
      Right (pmap,spTree) = pmapSpTree
      candidates          = edges pgraph \\ spTree                              -- remove all the spanningTree edges from the candidate edges, so that they dont apear again the in spanningTree if they disapear.
      (ratio,cycle)       = maximum $ zip (map mcrFromParametricCycle cycles) cycles        -- if initial tree already contains a cycle, this is the MCR

mcrFromParametricCycle :: ParametricEdges e
  => [e l]
  -> Maybe Weight
mcrFromParametricCycle c
  | tokens == 0 = Nothing
  | otherwise   = Just $ w/ (tokens  % 1)
    where 
      (tokens, w) = sum $ map pdistance c
  
mcrR :: (Ord l, Eq (e l), ParametricEdges e)
  => l
  -> [e l]
  -> [e l]
  -> (Maybe Weight, Maybe [e l])
mcrR root candidates spTree
  | null es               = (Nothing, Nothing)                    -- no now edges found to add, graph is not cyclic?
  | isJust ancestorPath   = (Just lambda, fmap (pivot:) ancestorPath)
  | otherwise             = mcrR root candidates' spTree'
  where
    pmapSpTree           = pmapAndSpanningTree spTree root 1                  -- Either returns a spanningTree (Right) or a list of cycles (Left)
    Left cycles          = pmapSpTree                                         -- This should never occur, since the incomming tree never has a cycle
    Right (pmap,_)       = pmapSpTree                                         -- pmap contains all the start keys of all the nodes
    edgeKeys             = zip (map (edgeKey pmap) candidates) candidates     -- edgeKeys is a list with all the calculated lambda's zipped with their corresponding edge.
    (es, _)              = partition (\(a, b) -> isJust a) edgeKeys           -- remove the Nothings from the list. so es contains the list of tuples with lambda
    (Just lambda, pivot) = maximumBy orderTuples es                           -- take the max of the tuplelist, so take the largest lambda and the corresponding edge (pivot)
    (v, w)               = (source pivot, target pivot)                       -- source and destination of the pivot edge
    candidates'          = (map snd es) \\ [pivot]                            -- the edge candidates for the next iteration are the ones that have a lambda which is smaller or equal than the pivot lambda
    spTree'              = pivot : filter (\e -> target e /= w) spTree        -- the new spanningTree is the old one, with the one edge replaced by the pivot edge
    orderTuples x y      = compare (fst x) (fst y)
    ancestorPath         = findPathInTreeEdgeList spTree w v
   

{-
pivot (pmap, gTree) es = edgeKeys
--  | null edgeKeys = Left []
--  | otherwise     = (pmap', graphTree')
    where
      edgeKeys = filter (\(_,w) -> isJust w) $ zip es $ map (edgeKey pmap) es
      (w',e) = maximum $ map swap $ map (fmap fromJust) edgeKeys -- Take maximum of lambda, TODO: check below the "current" lambda (or pick one?)
      isAn = isAncestor (edges gTree) (target e) (source e) -- check if, in the current graphTree, the destination node is an anchestor of the source node
-}    


{- 
    isAncestor uses dfs to determine if the source node is an anchestor of the destination node
    isAncestor edges a b = true if there is a path from b to a 
    TODO: optimize, because dfs is a bit overkill....
-}

isAncestor :: (Eq n, Edges e)
  => [e n]
  -> n
  -> n
  -> Bool
isAncestor es s d = length l > 0 || s == d -- path to source, or selfedge
  where
    dfs = snd $ dfsHO es d -- all paths from destination
    l = filter (\e -> target e == s) dfs -- filter path to source
--    pathFromDfs (p:ps) =
    


{-
    Computes for which value of lambda the distance from a to b via
    edge (a, b) becomes larger than the current distance to b.
-}
edgeKey :: (ParametricEdges e, Ord l)
  => (M.Map l ParametricDistance)
  -> e l
  -> Maybe Weight
edgeKey pmap edge
    | deltaTokens > 0 = Just (w / (deltaTokens % 1)) -- w = Ratio, so division is fine
    | otherwise       = Nothing
    where
      Just (pdistSource) = M.lookup (source edge) pmap
      Just (pdistTarget) = M.lookup (target edge) pmap
      (deltaTokens, w)   = pdistSource + pdistance edge - pdistTarget -- is the sign still working properly (mark is stored as postive number, ex (7-λ) = (1,7)

{-
    evalEdges evaluates a list of parametric edges with some multiplier l
-}
evalEdges :: ParametricEdges e
  => Weight
  -> [e l]
  -> [Edge l]
evalEdges l es
  = es'
    where
      es' = map (evalEdge l) es
      evalEdge la e = ParametricEdge s t w' (m,w)
        where
          s = source e
          t = target e
          (m,w) = pdistance e
          w' = w - (fromIntegral m) * la

{-
    evalGraph evaluates a parametric graph with some multiplier l
-}
evalGraph :: ParametricEdges e
  => Weight
  -> Graph (M.Map l n) [e l]
  -> Graph (M.Map l n) [Edge l]
evalGraph l (Graph ns es)
  = Graph ns (evalEdges l es)

{-
    pmapAndSpanningTree provides Either 
    a list of cycles 
    or a M.Map containing the parametric distance and the path, to/for each node
-}

pmapAndSpanningTree :: (Ord l, ParametricEdges e)
  => [e l]
  -> l
  -> Weight
  -> Either [[Edge l]] (M.Map l ParametricDistance, [Edge l])
pmapAndSpanningTree es root lambda
  | isLeft bf = Left cycles
  | otherwise = Right (pmap, es')
    where
      bf          = bellmanFord (evalEdges lambda es) root            -- all weights and paths from root node with a lambda
      Left cycles = bf                                                -- Left means a list of cycles
      Right bfMap = bf                                                -- Right means a M.Map containing weights and paths from root node to all other nodes
      paths       = map snd $ M.elems $ bfMap                         -- all paths from M.Map
      es'         = foldl union [] paths                              -- combine all paths to form a new graph
      pmap        = M.map (\(_,ps) -> sum $ map pdistance ps) bfMap   -- M.Map containing all the nodes with their parametric distance

{-
    df2parametricGraph converts a dataflow graph to a graph with parametric edges
-}

df2parametricGraph :: (Ord l, DFNodes n, DFEdges e)
  => Graph (M.Map l (n l)) [e l]
  -> Graph (M.Map l (n l)) [Edge l]
df2parametricGraph (Graph ns es)
  = Graph ns es'
    where
      es' = map edge2edge es
      edge2edge e = (ParametricEdge s t w (m,w))
        where
          s = source e
          t = target e
          m = tokens e
          Just n = M.lookup s ns
          w = maximum (wcet n) %1
