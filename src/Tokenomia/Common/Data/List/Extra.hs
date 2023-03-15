module Tokenomia.Common.Data.List.Extra
    ( mapLastWith
    , para
    , transpose
    ) where

import Control.Applicative                             ( ZipList(..) )


-- | map different functions on elements of a list depending on their position.
mapLastWith :: (a -> a) -> (a -> a) -> [a] -> [a]
mapLastWith f f' = para g []
  where
    g x [] _ = [f' x]
    g x _  r =  f  x : r

-- | list paramorphism, see foldr
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f z = go
  where
    go []     = z
    go (x:xs) = f x xs (go xs)

-- | transpose generalized to traversable
transpose :: (Traversable t) => t [a] -> [t a]
transpose xs
  | null  xs  = []
  | otherwise = getZipList $ traverse ZipList xs
