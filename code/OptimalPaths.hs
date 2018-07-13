module OptimalPaths where

import Data.Ratio
import qualified Data.Map as M

type Distance = Ratio Integer

class WeightedEdge a (Eq b) | a -> b where
    weight :: a -> Distance
    source :: a -> b
    target :: a -> b

{-
    TODO: implement Bellman-Ford (the simplest way, first)
-}
longestPaths :: (WeightedEdge a b) -> [a] -> b -> M.Map b a
longestPaths edges root
    = M.empty
    where
      (xs, ys)  =   

longestPaths' edges distances root
    | null xs       = distances
    | otherwise     = longestPaths' (zs ++ ys) distances' (target z)
    where
      outgoing edge = source edge == root
      relaxing edge = distance (source edge) + weight edge > distance (target edge)
      (xs, ys)      = L.partition (liftA2 outgoing relaxing) edges
      z:zs          = xs


