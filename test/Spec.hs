{-# LANGUAGE OverloadedStrings #-}
module Main(main) where


import qualified Spec.Tokenomia.Common.Data.Function.Memoize
import qualified Spec.Tokenomia.Common.Data.Sequence.IntegerPartitions
import Test.Tasty ( TestTree, defaultMain, testGroup )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "use cases"
    [ Spec.Tokenomia.Common.Data.Function.Memoize.tests
    , Spec.Tokenomia.Common.Data.Sequence.IntegerPartitions.tests
    ]
