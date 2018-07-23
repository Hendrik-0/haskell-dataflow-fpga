module DataFlow.Types 
( module Data.Ratio
, module DataFlow.Types
) where

import Graph

import qualified Data.Map as M
import Data.Ratio

data DFEdge n = HSDFEdge n n Integer
              |  SDFEdge n n Integer  Integer   Integer
              | CSDFEdge n n Integer [Integer] [Integer] 
                deriving Eq



data DFNode l = HSDFNode l  Integer
              | CSDFNode l [Integer]

--type DFGraph = Graph (M.Map Label DFNode) ([DFEdge Label]) 
type DFGraph l = Graph (M.Map l (DFNode l)) [DFEdge l]


instance Nodes DFNode where
  label (HSDFNode l _) = l
  label (CSDFNode l _) = l

class (Nodes n) => DFNodes n where
  wcet :: n l -> [Integer]
  period :: n l -> Integer

instance DFNodes DFNode where
  wcet (HSDFNode _ d) = [d]
  wcet (CSDFNode _ d) =  d
  period = fromIntegral . length . wcet



instance Edges DFEdge where
  source (HSDFEdge s _ _)     = s
  source ( SDFEdge s _ _ _ _) = s
  source (CSDFEdge s _ _ _ _) = s
  target (HSDFEdge _ t _)     = t
  target ( SDFEdge _ t _ _ _) = t
  target (CSDFEdge _ t _ _ _) = t



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



instance (Show l) => Show (DFNode l) where
  show n = show (label n)


