module Dotp where

import Clash.Prelude
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

{-# NOINLINE m #-}
m :: Num a => a -> a
m = (*9)

mm :: (Ord c, Enum c, Num c, Num a, KnownNat n1, KnownNat n2)
  => (Vec n1 a, Vec n2 a, c)
  -> Maybe (Vec (n1 + 1) a)
  -> ((Vec n1 a, Vec n2 a, c), Maybe (Vec (n2 + 1) a))
mm (usi, uso, -1) Nothing = ((usi,  uso,  -1), Nothing)
mm (usi, uso, c)  i = ((usi', uso', c'),o)
  where
    fo = m x

    x = case i of
      Just inp -> head inp
      _        -> usi!!c

    usi' = case i of
      Just inp -> tail inp
      _        -> usi

    uso' = case c >= 2 of
      True  -> uso
      False -> replace c' fo uso

    c' = case (i,c >= 2) of
      (Just _, _) -> 0
      (_, False)  -> c + 1
      (_, True)   -> -1

    o = case (i,c >= 2) of
      (Just _, _) -> Nothing
      (_, False)  -> Nothing
      (_, True)   -> Just (uso ++ singleton fo)

mmealy = mealy mm (repeat 0 :: Vec 3 (Signed 8), repeat 0 :: Vec 3 (Signed 8), -1 :: Signed 3)

{-# NOINLINE l #-}
l o = case o of
  Just l -> Just $ l + 1
  Nothing -> Just 1

{-# NOINLINE f #-}
f = (fold (+) <$>)

system i = o1
  where
    xs = mmealy i
    o = f <$> xs
    o1 = l <$> o

topEntity :: Clk -> Rst -> Sig (Maybe (Vec 4 (Signed 8))) -> Sig (Maybe (Signed 8))
topEntity clk rst i = exposeClockResetEnable system clk rst enableGen i

--topEntity :: Clk -> Rst -> Sig (Maybe (Vec 4 (Signed 8))) -> Sig (Maybe (Vec 4 (Signed 8)))
--topEntity clk rst i = exposeClockResetEnable mmealy clk rst enableGen i



-- simulation function which simulates the same function as you put into the mealy machine
--  and generates a string which you can print with putStr to have a nicer layout
sim f s [] = []
sim f s (i:is) = h L.++ sim f s' is where
  (s', o) = f s i
  h = "s'=  " L.++ (show s') L.++ ",\t\to = " L.++ (show o) L.++ "\r\n"