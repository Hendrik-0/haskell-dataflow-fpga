module Hardware.Clock where

import qualified Data.Map as M
import Data.Maybe

import DataFlow
import Graph

-- clock is gcd (M/r0, M/r1, ..., M/rn) * mcr
-- total period is M*mcr
clock g | isJust mcr = Just (cl,p)
        | otherwise  = Nothing where
  m = modulus g
  rv = repetitionVector g             -- repetition vector r (r0,r1,...,rn)
  rvm = map (m %) (M.elems rv)        -- (M/r0, M/r1, ..., M/rn)
  numerators = map numerator rvm      -- all the numerators of the repetition vector
  denominators = map denominator rvm  -- all the denominators of the repetition vector
  gcdn = foldl1 gcd numerators        -- gcd of numerators 
  lcmd = foldl1 lcm denominators      -- lcm of denominators
  (mcr,_) = maxCycleRatio $ singleRateApx g
  mcr' = fromJust mcr
  cl = (gcdn % lcmd) * mcr'           -- clock period, gcd of ratios is gcd numerators / lcm denominators
  p = mcr' * (m%1)                    -- period of sps
