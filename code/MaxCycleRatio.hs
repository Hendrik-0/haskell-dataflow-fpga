module MaxCycleRatio where

import Data.Ratio
import Data.Maybe

type ParametricDistance = (Integer, Ratio Integer)

data WeightedEdge a = Edge (a, a) (Ratio Integer)

class WeightedMarkedEdge w where
    weight :: w a -> Ratio Integer
    marking :: w a -> Integer
    source :: w a -> a
    target :: w a -> a
    pdistance :: w a -> ParametricDistance

instance WeightedMarkedEdge WeightedEdge where
    target (Edge (x, y) _) = y
    source (Edge (x, y) _) = x
    weight (Edge _ w) = w
    marking e = 0
    pdistance = weight

instance (Num a, Num b) => Num (a,b) where
   fromInteger a = (fromInteger a, fromInteger a)
   (a,b) + (a',b') = (a + a', b + b')
   (a,b) - (a',b') = (a - a', b - b')
   (a,b) * (a',b') = (a * a', b * b')
   negate (a,b) = (negate a, negate b)
   abs (a,b) = (abs a, abs b)
   signum (a,b) = (signum a, signum b)

eval :: (WeightedMarkedEdge a) => Ratio Integer -> a b -> WeightedEdge b
eval at edge
    = Edge (source edge, target edge) (weight edge - m * at)
    where
      m = fromIntegral (marking edge)

{-
    TODO: adapt for non-strongly-connected graphs (apply to all strongly connected components)
-}
mcr :: WeightedMarkedEdge a => [a b] -> ([a b], Maybe (Ratio Integer))
mcr edges
    = f $ mcrR tree edges
    where
      tree = feasibleTree edges :: M.Map b (a b, ParametricDistance)
      f (Left cycle) = (cycle, Just $ sum (map weight cycle) % sum (map marking cycle))

mcrR :: Either [a] LongestParametricPathsTree -> [a] -> Either [a] LongestParametricPathsTree
mcrR (Left cycle) edges = Left cycle
mcrR (Right tree) edges = mcrR (pivot tree edges) edges
      
{-
    TODO: implement isAncestor, extract cycle from tree in case there is an ancestor
-}
pivot :: WeightedMarkedEdge a => LongestParametricPathsTree -> [a] -> Either [a] LongestParametricPathsTree
pivot tree edges
    | null edgeKeys = Left []
    | otherwise     = tree'
    where
      edgeKeys     = filter isJust $ map (edgeKey tree) edges 
      (Just e : _) = edgeKeys
      tree'        | isAncestor tree (target e) (source e) = Left cycle
                   | otherwise                             = Right $ propagate $ M.insert (target e) (e, pdistSource + pdistance edge)

-- TODO Hendrik
-- propagate the longest distances from the root of the tree all the way down to the leaves
-- update the tree structure
propagate:: LongestParametricPathsTree -> Maybe LongestParametricPathsTree
propagate t = Just t

{-
    Computes for which value of lambda the distance from a to b via
    edge (a, b) becomes larger than the current distance to b.
-}
edgeKey :: WeightedMarkedEdge a => LongestParametricPathsTree -> a -> Maybe (Ratio Integer)
edgeKey tree edge
    | deltaTokens > 0 = Just (w % lambdas)
    | otherwise       = Nothing
    where
      Just (_, pdistSource) = M.lookup (source edge) tree
      Just (_, pdistTarget) = M.lookup (target edge) tree
      (deltaTokens, w)      = pdistSource + pdistance edge - pdistTarget

feasibleTree edges
    = longestPathsTree weightedEdges
    where
      m             = sum [weight edge | edge <- edges] + 1 -- dit is gewoon "een groot getal" (HF)
      weightedEdges = map (eval m)
