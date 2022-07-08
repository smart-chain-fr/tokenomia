module Spec.Tokenomia.Common.Data.Function.Memoize
    ( tests
    ) where

--
-- This module tests the `memoize` function, with no regard to performance.
-- The memoized function must equal its corresponding unmemoized function.
--

import Test.Tasty               ( TestTree, testGroup )
import Test.Tasty.HUnit         ( testCase, (@?=) )
import Test.Tasty.QuickCheck    ( chooseInteger, testProperty, withMaxSuccess )

import Tokenomia.Common.Data.Function.Memoize
    ( memoize )


-- | Memoize with explicit open recursion
fibonacci :: Integer -> Integer
fibonacci = memoize f_
  where
    f_ :: (Integer -> Integer) -> Integer -> Integer
    f_ _   0 = 0
    f_ _   1 = 1
    f_ rec n = rec (n - 2) + rec (n - 1)

-- | Exponential recursion
fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = fibonacci' (n - 2) + fibonacci' (n - 1)

-- | Memoize with explicit open recursion
f :: Integer -> Integer
f = memoize f_
  where
    f_ :: (Integer -> Integer) -> Integer -> Integer
    f_ _   0 = 0
    f_ rec n =
        max n
            $ rec (n `div` 2)
            + rec (n `div` 3)
            + rec (n `div` 4)

-- | Exponential recursion
g :: Integer -> Integer
g 0 = 0
g n =
    max n
        $ g (n `div` 2)
        + g (n `div` 3)
        + g (n `div` 4)

tests :: TestTree
tests = testGroup "Common.Data.Function.Memoize" [unitTests, properties]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "fibonacci  0 == 0"         $ fibonacci  0 @?= 0
    , testCase "fibonacci  1 == 1"         $ fibonacci  1 @?= 1
    , testCase "fibonacci  2 == 1"         $ fibonacci  2 @?= 1
    , testCase "fibonacci  3 == 2"         $ fibonacci  3 @?= 2
    , testCase "fibonacci  4 == 3"         $ fibonacci  4 @?= 3
    , testCase "fibonacci  5 == 5"         $ fibonacci  5 @?= 5
    , testCase "fibonacci  6 == 8"         $ fibonacci  6 @?= 8
    , testCase "fibonacci  7 == 13"        $ fibonacci  7 @?= 13
    , testCase "fibonacci 40 == 102334155" $ fibonacci 40 @?= 102334155
    ]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "f == memoized f,  f := fibonacci" $ withMaxSuccess 30 $
        do
            n <- chooseInteger (0, 30)
            return $ fibonacci n == fibonacci' n
  , testProperty "f == memoized f,  f n := max(n, f n/2 + f n/3 + f n/4)" $
        do
            n <- chooseInteger (0, 1000)
            return $ f n == g n
  ]
