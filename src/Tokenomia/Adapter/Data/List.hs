module Tokenomia.Adapter.Data.List
  (removeDuplicates) where
import Data.List 

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = Prelude.map Prelude.head . group . sort

