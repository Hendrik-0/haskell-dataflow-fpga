module GraphTypes where

import qualified Data.Map as M
import Data.Ratio

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
    production (HSDFEdge {}) = [1]
    production (SDFEdge {pr = pr}) = [pr]
    production (CSDFEdge {prv = pr}) = pr
    consumption (HSDFEdge {}) = [1]
    consumption (SDFEdge {cr = cr}) = [cr]
    consumption (CSDFEdge {crv = cr}) = cr
    tokens = tks
    prate edge = sum p % (fromIntegral $ length p) where p = production edge
    crate edge = sum p % (fromIntegral $ length p) where p = consumption edge
--    prate edge = let p = production edge in sum p % (fromIntegral $ length p)
--    crate edge = let c = consumption edge in sum c % (fromIntegral $ length c)



instance Show Edge where
  show e = [(ns e)] ++ "--->" ++ [(nd e)]

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
