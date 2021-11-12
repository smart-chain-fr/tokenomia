module Tokenomia.Adapter.Data.List where
import qualified Data.Set as Set

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

short :: Ord a => a -> a
short [] = []
short x = take (length x `div` 2) x