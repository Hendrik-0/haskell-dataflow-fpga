module GraphTypes where

import qualified Data.Map as M
import Data.Ratio


type Weight = Ratio Integer
type Mark = Integer
type ParametricDistance = (Mark, Weight)

data Node = Node Label
data Edge n = Edge n n
            | WeightedEdge n n Weight
            | WeightedMarkedEdge n n Weight Mark
              deriving Eq


data DFEdge n = HSDFEdge n n Integer
              |  SDFEdge n n Integer  Integer   Integer
              | CSDFEdge n n Integer [Integer] [Integer] 
                deriving Eq

type Label = Char

data DFNode = HSDFNode Label  Integer
            | CSDFNode Label [Integer]

type DFGraph = Graph (M.Map Label DFNode) ([DFEdge Label]) 
type WeightedGraph = Graph (M.Map Label Node) ([Edge Label])

data Graph n e = Graph n e

class Graphs g where
  nodes :: g n e -> n
  edges :: g n e -> e

instance Graphs Graph where
  nodes (Graph n _) = n
  edges (Graph _ e) = e



class Nodes n where
  label :: n -> Label

instance Nodes Node where
  label (Node n) = n

instance Nodes DFNode where
  label (HSDFNode l _) = l
  label (CSDFNode l _) = l


class (Nodes n) => DFNodes n where
  wcet :: n -> [Integer]
  period :: n -> Integer

instance DFNodes DFNode where
  wcet (HSDFNode _ d) = [d]
  wcet (CSDFNode _ d) =  d
  period = fromIntegral . length . wcet


class Edges e where
  source :: e n -> n
  target :: e n -> n

instance Edges Edge where
  source (Edge s _)                   = s 
  source (WeightedEdge s _ _)         = s
  source (WeightedMarkedEdge s _ _ _) = s
  target (Edge _ t)                   = t
  target (WeightedEdge _ t _)         = t
  target (WeightedMarkedEdge _ t _ _) = t

instance Edges DFEdge where
  source (HSDFEdge s _ _)     = s
  source ( SDFEdge s _ _ _ _) = s
  source (CSDFEdge s _ _ _ _) = s
  target (HSDFEdge _ t _)     = t
  target ( SDFEdge _ t _ _ _) = t
  target (CSDFEdge _ t _ _ _) = t



class (Edges e) => WeightedEdges e where
   weight :: e n -> Weight

instance WeightedEdges Edge where
   weight (WeightedEdge _ _ w)         = w
   weight (WeightedMarkedEdge _ _ w _) = w
   weight _                            = 1 -- TODO ? default edge has weight 1



class (Edges e, WeightedEdges e) => WeightedMarkedEdges e where
  mark :: e n -> Mark
  pdistance :: e n -> ParametricDistance

instance WeightedMarkedEdges Edge where
  mark (WeightedMarkedEdge _ _ _ m) = m
  mark _                            = 0 -- TODO? marking 0 for Edge and WeightedEdge => no lambdas)
  pdistance e = (mark e, weight e)






class (Edges e) => DFEdges e where
  tokens :: e n -> Integer
  production :: e n -> [Integer]
  consumption :: e n -> [Integer]
  prate :: e n -> Ratio Integer
  crate :: e n -> Ratio Integer

instance DFEdges DFEdge where
  tokens (HSDFEdge _ _ t)     = t
  tokens ( SDFEdge _ _ t _ _) = t
  tokens (CSDFEdge _ _ t _ _) = t
  production  (SDFEdge _ _ _ p _)  = [p]
  production (CSDFEdge _ _ _ p _)  =  p
  production _                     = [1]
  consumption  (SDFEdge _ _ _ _ c) = [c]
  consumption (CSDFEdge _ _ _ _ c) =  c
  consumption _                    = [1]
  prate edge = sum p % (fromIntegral $ length p) where p = production edge
  crate edge = sum c % (fromIntegral $ length c) where c = consumption edge

instance (Show n) => Show (DFEdge n) where
  show (HSDFEdge s d t) = (show s) ++ "--" ++ (show t) ++ "-->" ++ (show d)
  show (SDFEdge s d t pr cr) = (show s) ++ (show pr)  ++ "--" ++ (show t) ++ "-->" ++ (show cr) ++ (show d)
  show (CSDFEdge s d t prv crv) = (show s) ++ (show prv) ++ "--" ++ (show t) ++ "-->" ++ (show crv) ++ (show d)

instance (Show n) => Show (Edge n) where
  --show e = (show $ source e) ++ "-->" ++ (show $ target e)
  show (Edge s d) = (show s) ++ "-->" ++ (show d)
  show (WeightedEdge s d w) = (show s) ++ "--(" ++ (show w) ++ ")-->" ++ (show d)
  show e@(WeightedMarkedEdge s d w m) = (show s) ++ "--(" ++ sw ++ "-" ++ (show l) ++ "l)-->" ++ (show d) 
    where
      (l,w) = pdistance e
      sw | denominator w == 1 = show (numerator w)
         | otherwise          = show (w)

instance Show Node where
  show (Node n) = [n]



instance (Ord n) => Ord (Edge n) where
  compare a b = compare (weight a) (weight b)


instance (Num a, Num b) => Num (a,b) where
   fromInteger a   = (fromInteger a, fromInteger a)
   (a,b) + (a',b') = (a + a', b + b')
   (a,b) - (a',b') = (a - a', b - b')
   (a,b) * (a',b') = (a * a', b * b')
   negate (a,b)    = (negate a, negate b)
   abs (a,b)       = (abs a, abs b)
   signum (a,b)    = (signum a, signum b)



{-

data WeightedEdge a = WeightedEdge (a, a) (Ratio Integer)

type Label = Char

data Node =   HSDFNode { lb :: Label
                       , wcet :: Integer
                       }
            | CSDFNode { lb :: Label
                       , wcetv :: [Integer]
                       } deriving Eq


data Edge =   HSDFEdge  { ns :: Label
                        , nd :: Label
                        , tks :: Integer
                        }
            | SDFEdge   { ns :: Label
                        , nd :: Label
                        , tks :: Integer
                        , pr :: Integer
                        , cr :: Integer
                        } 
            | CSDFEdge  { ns :: Label
                        , nd :: Label
                        , prv :: [Integer]
                        , crv :: [Integer]
                        , tks :: Integer
                        } deriving Eq

data Graph =  Graph { nodes :: M.Map Label Node
                    , edges :: [Edge]
                    } 

class CSDFActor a where
    wcets :: a -> [Integer]
    period :: a -> Integer

instance CSDFActor Node where
    wcets (HSDFNode {wcet = w}) = [w]
    wcets (CSDFNode {wcetv = ws}) = ws
    period = fromIntegral . length . wcets

class CSDFChannel a where
    production :: a -> [Integer]
    consumption :: a -> [Integer]
    tokens :: a -> Integer
    prate :: a -> Ratio Integer
    crate :: a -> Ratio Integer

instance CSDFChannel Edge where
    production  (HSDFEdge {      }) = [1]
    production  ( SDFEdge { pr=pr}) = [pr]
    production  (CSDFEdge {prv=pr}) = pr
    consumption (HSDFEdge {      }) = [1]
    consumption ( SDFEdge { cr=cr}) = [cr]
    consumption (CSDFEdge {crv=cr}) = cr
    tokens = tks
    prate edge = sum p % (fromIntegral $ length p) where p = production edge
    crate edge = sum p % (fromIntegral $ length p) where p = consumption edge
--    prate edge = let p = production edge in sum p % (fromIntegral $ length p)
--    crate edge = let c = consumption edge in sum c % (fromIntegral $ length c)



instance Show Edge where
  --show e = [(ns e)] ++ "--->" ++ [(nd e)]
  show (HSDFEdge {ns=ns, nd=nd,                   tks=tks}) = [ns] ++               "--" ++ (show tks) ++ "-->"               ++ [nd]
  show ( SDFEdge {ns=ns, nd=nd,  pr=pr ,  cr=cr , tks=tks}) = [ns] ++ (show pr)  ++ "--" ++ (show tks) ++ "-->" ++ (show cr)  ++ [nd]
  show (CSDFEdge {ns=ns, nd=nd, prv=prv, crv=crv, tks=tks}) = [ns] ++ (show prv) ++ "--" ++ (show tks) ++ "-->" ++ (show crv) ++ [nd]

instance Show Node where
  show n = [lb n]


-- example graphs:
sdf  = Graph {nodes = M.fromList [('I', HSDFNode {lb = 'I', wcet = 0})
                      , ('*', HSDFNode {lb = '*', wcet = 1})
                      , ('f', HSDFNode {lb = 'f', wcet = 4})
                      , ('Z', HSDFNode {lb = 'Z', wcet = 0})
                      ],
              edges = [ SDFEdge {ns = 'I', nd = '*', pr = 12, cr = 1, tks = 0}
                      , SDFEdge {ns = 'I', nd = '*', pr = 12, cr = 1, tks = 0}
                      , SDFEdge {ns = '*', nd = 'f', pr = 1 , cr = 4, tks = 0}
                      , SDFEdge {ns = 'f', nd = 'Z', pr = 1 , cr = 3, tks = 0}
                      , SDFEdge {ns = 'Z', nd = 'I', pr = 1 , cr = 1, tks = 1}
                      ]
             }

csdf = Graph {nodes = M.fromList    -- modulus = 36
                      [ ('a', CSDFNode {lb = 'a', wcetv = [2,3]})                           -- q = 4
                      , ('b', CSDFNode {lb = 'b', wcetv = [2,2]})                           -- q = 6
                      , ('c', CSDFNode {lb = 'c', wcetv = [3,4]})                           -- q = 6
                      ],
              edges = [ CSDFEdge {ns = 'a', nd = 'a', prv = [1,1], crv = [1,1], tks = 1}    -- weight = 9
                      , CSDFEdge {ns = 'a', nd = 'b', prv = [1,2], crv = [1,1], tks = 0}    -- weight = 6
                      , CSDFEdge {ns = 'b', nd = 'a', prv = [2,0], crv = [2,1], tks = 2}    -- weight = 6
                      , CSDFEdge {ns = 'b', nd = 'c', prv = [1,2], crv = [2,1], tks = 0}    -- weight = 4
                      , CSDFEdge {ns = 'c', nd = 'b', prv = [2,1], crv = [1,2], tks = 3}    -- weight = 4
                      ]
             }

hsdf :: Graph
hsdf = Graph {nodes = M.fromList
                    [('a', HSDFNode {lb = 'a', wcet = 1})
                    ,('b', HSDFNode {lb = 'b', wcet = 1})
                    ,('c', HSDFNode {lb = 'c', wcet = 1})
                    ,('d', HSDFNode {lb = 'd', wcet = 1})
                    ,('e', HSDFNode {lb = 'e', wcet = 1})
                    ,('f', HSDFNode {lb = 'f', wcet = 1})
                    ],
            edges = [ HSDFEdge {ns = 'a', nd = 'b', tks = 0}
                    , HSDFEdge {ns = 'a', nd = 'c', tks = 0}
                    , HSDFEdge {ns = 'a', nd = 'd', tks = 0}
                    , HSDFEdge {ns = 'b', nd = 'e', tks = 0}
                    , HSDFEdge {ns = 'd', nd = 'e', tks = 0}
                    , HSDFEdge {ns = 'd', nd = 'f', tks = 0}
                    , HSDFEdge {ns = 'e', nd = 'f', tks = 0}
                    , HSDFEdge {ns = 'f', nd = 'b', tks = 0}
                    , HSDFEdge {ns = 'e', nd = 'c', tks = 0}
                    , HSDFEdge {ns = 'b', nd = 'a', tks = 0}
                    ]
           }
-}
