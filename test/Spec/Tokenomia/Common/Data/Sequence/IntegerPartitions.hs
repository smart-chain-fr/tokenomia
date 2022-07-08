module Spec.Tokenomia.Common.Data.Sequence.IntegerPartitions
    ( tests
    ) where

--
-- This module tests the `integerParitions` function, with no regard to performance.
-- The memoized function must equal its corresponding unmemoized function.
--

import Test.Tasty               ( TestTree, testGroup )
import Test.Tasty.HUnit         ( testCase, (@?=) )
import Test.Tasty.QuickCheck    ( chooseInteger, testProperty, withMaxSuccess )

import Data.List.NonEmpty       ( NonEmpty((:|)) )

import Tokenomia.Common.Data.Sequence.IntegerPartitions
    ( Partition(..)
    , triangular
    , fromCoordinates
    , toCoordinates
    , integerPartitions'
    , integerPartitions
    )


-- | Alternative implementation in O(n^(1/2))
--
-- toCoordinates :: Integer -> (Integer, Integer)
-- toCoordinates 0 = (0, 0)
-- toCoordinates x =
--     let xs = zip (triangular <$> [1..]) [1..]
--         ys = dropWhile (\(i, _) -> i<=x) xs
--         n = snd (head ys) - 1
--         k = x - triangular n
--     in
--         (n, k)

tests :: TestTree
tests = testGroup "Common.Data.Sequence.IntegerPartitions" [ unitTests, properties ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "triangular sequence"
        [ testCase "triangular 0 == 0"  $ triangular  0 @?= 0
        , testCase "triangular 1 == 1"  $ triangular  1 @?= 1
        , testCase "triangular 2 == 3"  $ triangular  2 @?= 3
        , testCase "triangular 3 == 6"  $ triangular  3 @?= 6
        , testCase "triangular 4 == 10" $ triangular  4 @?= 10
        , testCase "triangular 5 == 15" $ triangular  5 @?= 15
        , testCase "triangular 6 == 21" $ triangular  6 @?= 21
        , testCase "triangular 7 == 28" $ triangular  7 @?= 28
        ]
    , testGroup "fromCoordinates"
        [ testCase "fromCoordinates (0, 0) == 0" $ fromCoordinates (0, 0) @?= 0
        , testCase "fromCoordinates (1, 0) == 1" $ fromCoordinates (1, 0) @?= 1
        , testCase "fromCoordinates (1, 1) == 2" $ fromCoordinates (1, 1) @?= 2
        , testCase "fromCoordinates (2, 0) == 3" $ fromCoordinates (2, 0) @?= 3
        , testCase "fromCoordinates (2, 1) == 4" $ fromCoordinates (2, 1) @?= 4
        , testCase "fromCoordinates (2, 2) == 5" $ fromCoordinates (2, 2) @?= 5
        ]
    , testGroup "toCoordinates"
        [ testCase "toCoordinates 0 == (0, 0)" $ toCoordinates 0 @?= (0, 0)
        , testCase "toCoordinates 1 == (1, 0)" $ toCoordinates 1 @?= (1, 0)
        , testCase "toCoordinates 2 == (1, 1)" $ toCoordinates 2 @?= (1, 1)
        , testCase "toCoordinates 3 == (2, 0)" $ toCoordinates 3 @?= (2, 0)
        , testCase "toCoordinates 4 == (2, 1)" $ toCoordinates 4 @?= (2, 1)
        , testCase "toCoordinates 5 == (2, 2)" $ toCoordinates 5 @?= (2, 2)
        ]
    , testGroup "integerPartitions"
        [ testCase "integerPartitions 1" $ integerPartitions 1 @?=
            []
        , testCase "integerPartitions 2" $ integerPartitions 2 @?=
            [ Partition (2 :| []) ]
        , testCase "integerPartitions 3" $ integerPartitions 3 @?=
            [ Partition (3 :| []) ]
        , testCase "integerPartitions 4" $ integerPartitions 4 @?=
            [ Partition (4 :| [])
            , Partition (2 :| [2])
            ]
        , testCase "integerPartitions 5" $ integerPartitions 5 @?=
            [ Partition (5 :| [])
            , Partition (3 :| [2])
            ]
        , testCase "integerPartitions 6" $ integerPartitions 6 @?=
            [ Partition (6 :| [])
            , Partition (4 :| [2])
            , Partition (3 :| [3])
            , Partition (2 :| [2,2])
            ]
        , testCase "integerPartitions 6" $ integerPartitions 7 @?=
            [ Partition (7 :| [])
            , Partition (5 :| [2])
            , Partition (4 :| [3])
            , Partition (3 :| [2,2])
            ]
        , testCase "integerPartitions 8" $ integerPartitions 8 @?=
            [ Partition (8 :| [])
            , Partition (6 :| [2])
            , Partition (5 :| [3])
            , Partition (4 :| [4])
            , Partition (4 :| [2,2])
            , Partition (3 :| [3,2])
            , Partition (2 :| [2,2,2])
            ]
        , testCase "length (integerPartitions 40) == 6153"  $ length (integerPartitions 40) @?= 6153
        , testCase "length (integerPartitions 41) == 7245"  $ length (integerPartitions 41) @?= 7245
        , testCase "length (integerPartitions 42) == 8591"  $ length (integerPartitions 42) @?= 8591
        , testCase "length (integerPartitions 43) == 10087" $ length (integerPartitions 43) @?= 10087
        , testCase "length (integerPartitions 44) == 11914" $ length (integerPartitions 44) @?= 11914
        , testCase "length (integerPartitions 45) == 13959" $ length (integerPartitions 45) @?= 13959
        , testCase "length (integerPartitions 46) == 16424" $ length (integerPartitions 46) @?= 16424
        , testCase "length (integerPartitions 47) == 19196" $ length (integerPartitions 47) @?= 19196
        , testCase "length (integerPartitions 48) == 22519" $ length (integerPartitions 48) @?= 22519
        , testCase "length (integerPartitions 49) == 26252" $ length (integerPartitions 49) @?= 26252
        , testCase "length (integerPartitions 50) == 30701" $ length (integerPartitions 50) @?= 30701
        ]
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "bijection laws"
        [ testProperty "fromCoordinates . toCoordinates == id" $
              do
                  n <- chooseInteger (0, 1000)
                  k <- chooseInteger (0, n)
                  return $ toCoordinates (fromCoordinates (n, k)) == (n, k)
        , testProperty "toCoordinates . fromCoordinates == id" $
              do
                  x <- chooseInteger (0, 1000000)
                  return $ fromCoordinates (toCoordinates x) == x
        ]
    , testGroup "correct memoization"
        [ testProperty "f == memoized f, f := integerPartitions" $ withMaxSuccess 30 $
              do
                  n <- chooseInteger (0, 50)
                  return $ integerPartitions' n == integerPartitions n
        ]
    ]
