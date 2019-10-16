module Graph.Types
( module Data.Ratio
, module Graph.Types
) where

import qualified Data.Map as M
import Data.Ratio


type Weight = Ratio Integer
type Mark = Integer
type ParametricDistance = (Mark, Weight)

data Node l = Node l

data Edge n = Edge n n
            | WeightedEdge n n Weight
            | WeightedMarkedEdge n n Weight Mark
            | ParametricEdge n n Weight ParametricDistance
              deriving Eq

data Graph n e = Graph n e


--type WeightedGraph = Graph (M.Map Label Node) ([Edge Label])


class Graphs g where
  nodes :: g n e -> n
  edges :: g n e -> e

instance Graphs Graph where
  nodes (Graph n _) = n
  edges (Graph _ e) = e






class Nodes n where
  label :: n l -> l

instance Nodes Node where
  label (Node n) = n


class Edges e where
  source :: e n -> n
  target :: e n -> n

instance Edges Edge where
  source (Edge s _)                   = s
  source (WeightedEdge s _ _)         = s
  source (WeightedMarkedEdge s _ _ _) = s
  source (ParametricEdge s _ _ _)     = s

  target (Edge _ t)                   = t
  target (WeightedEdge _ t _)         = t
  target (WeightedMarkedEdge _ t _ _) = t
  target (ParametricEdge _ t _ _)     = t



class (Edges e) => WeightedEdges e where
   weight :: e n -> Weight

instance WeightedEdges Edge where
   weight (WeightedEdge _ _ w)         = w
   weight (WeightedMarkedEdge _ _ w _) = w
   weight (ParametricEdge _ _ w _)     = w
   weight _                            = 1 -- TODO ? default edge has weight 1






class (Edges e, WeightedEdges e) => WeightedMarkedEdges e where
  mark :: e n -> Mark

instance WeightedMarkedEdges Edge where
  mark (WeightedMarkedEdge _ _ _ m) = m
  mark (ParametricEdge _ _ _ (m,w)) = m
  mark _                            = 0 -- TODO? marking 0 for Edge and WeightedEdge => no lambdas)





class (Edges e, WeightedEdges e, WeightedMarkedEdges e) => ParametricEdges e where
  pdistance :: e n -> ParametricDistance

instance ParametricEdges Edge where
  pdistance (ParametricEdge _ _ _ p) = p
  pdistance e = (mark e, weight e)





instance (Show n) => Show (Edge n) where
  --show e = (show $ source e) ++ "-->" ++ (show $ target e)
  show (Edge s d) = (show s) ++ "-->" ++ (show d) ++ "\n"
  show (WeightedEdge s d w) = (show s) ++ "--(" ++ (show w) ++ ")-->" ++ (show d) ++ "\n"
  show (WeightedMarkedEdge s d w m)  = (show s) ++ "--" ++ (show (m,w)) ++ "-->" ++ (show d) ++ "\n"
  show (ParametricEdge s d w' (m,w))
    = (show s) ++ "--(" ++ sw' ++ ")(" ++ sw ++ ")-->" ++ (show d) ++ "\n"
      where
        sw  | denominator w  == 1 = show (numerator w ) ++ "-" ++ (show m) ++ "l" --"λ"
            | otherwise           = show (          w ) ++ "-" ++ (show m) ++ "l" --"λ"
        sw' | denominator w' == 1 = show (numerator w')
            | otherwise           = show (          w')

instance (Show n) => Show (Node n) where
  show (Node n) = show n


instance (Show n, Show e) => Show (Graph n e) where
  show (Graph ns es) = "Nodes: " ++ (show ns) ++ "| Edges: " ++ show es







--instance (Ord n) => Ord (Edge n) where
--  compare a b = compare (weight a) (weight b)


instance (Num a, Num b) => Num (a,b) where
   fromInteger a   = (fromInteger a, fromInteger a)
   (a,b) + (a',b') = (a + a', b + b')
   (a,b) - (a',b') = (a - a', b - b')
   (a,b) * (a',b') = (a * a', b * b')
   negate (a,b)    = (negate a, negate b)
   abs (a,b)       = (abs a, abs b)
   signum (a,b)    = (signum a, signum b)



edgesFromNode :: (Eq l, Edges e) => l -> [e l] -> [e l]
edgesFromNode n es = filter (\e -> (source e) == n) es


edgesToNode :: (Eq l, Edges e) => l -> [e l] -> [e l]
edgesToNode n es = filter (\e -> (target e) == n) es