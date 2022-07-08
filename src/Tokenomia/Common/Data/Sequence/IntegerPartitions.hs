module Tokenomia.Common.Data.Sequence.IntegerPartitions
    ( Partition(..)
    , triangular
    , fromCoordinates
    , toCoordinates
    , integerPartitions'
    , integerPartitions
    ) where

--
-- Functions related to integer partitions of n that do not contain 1 as a part.
--
-- This module provides both
--     integerPartitions' — an exponential recursion
-- and integerPartitions  — a slighter faster memoized implementation.
--

import Data.List.NonEmpty       ( NonEmpty((:|)), cons )

import Tokenomia.Common.Data.Function.Memoize
    ( memoize )


newtype Partition a
    =   Partition ( NonEmpty a )
    deriving ( Show, Eq )

triangular :: Integer -> Integer
triangular n = n * (n + 1) `div` 2

fromCoordinates :: (Integer, Integer) -> Integer
fromCoordinates (n, k) = triangular n + k

toCoordinates :: Integer -> (Integer, Integer)
toCoordinates x =
    let m :: Double
        m = sqrt . fromIntegral $ 8 * x + 1
        n = floor $ (m - 1) / 2
        k = x - (triangular n - 1) - 1
    in
        (n, k)

-- | Exponential recursion listing all integer partitions of n starting with k not containing 1
integerPartitionsPrefixedBy' :: Integer -> Integer -> [Partition Integer]
integerPartitionsPrefixedBy' n k
    | k < 2      = []
    | n < 2      = []
    | n < k      = []
    | n == k     = [singleton n]
    | n == k + 1 = []
    | otherwise  =
        let m = min k (n - k)
        in
                prepend k
            <$> concat [integerPartitionsPrefixedBy' (n - k) i | i <- [m, m - 1 .. 2]]

-- | Wrapper listing all integer partitions of n not containing 1
integerPartitions' :: Integer -> [Partition Integer]
integerPartitions' n =
    concat [integerPartitionsPrefixedBy' n k | k <- [n, n - 1 .. 2]]

-- | Memoize with explicit open recursion integer partitions of n starting with k not containing 1
integerPartitionsPrefixedBy :: Integer -> [Partition Integer]
integerPartitionsPrefixedBy = memoize f
  where
    f :: (Integer -> [Partition Integer]) -> Integer -> [Partition Integer]
    f rec x
        | k < 2      = []
        | n < 2      = []
        | n < k      = []
        | n == k     = [singleton n]
        | n == k + 1 = []
        | otherwise  =
            let m = min k (n - k)
            in
                prepend k
            <$> concat [rec $ fromCoordinates (n - k, i) | i <- [m, m - 1 .. 2]]
      where
        (n, k) = toCoordinates x

-- | Wrapper for the memoized computation listing all integer partitions of n not containing 1
integerPartitions :: Integer -> [Partition Integer]
integerPartitions n =
    concat [integerPartitionsPrefixedBy $ fromCoordinates (n, k) | k <- [n, n - 1 .. 2]]

singleton :: a -> Partition a
singleton a = Partition (a :| [])

prepend :: a -> Partition a -> Partition a
prepend x (Partition xs) = Partition (x `cons` xs)
