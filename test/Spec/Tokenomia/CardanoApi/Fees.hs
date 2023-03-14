{-# LANGUAGE OverloadedStrings                         #-}

module Spec.Tokenomia.CardanoApi.Fees
    ( tests
    ) where

import Test.Tasty                                      ( TestTree, testGroup )
import Test.Tasty.HUnit                                ( Assertion, testCase, (@?=) )

import Cardano.Api
    ( AssetId(..)
    , AssetName(..)
    , Lovelace(..)
    , PolicyId(..)
    , Quantity
    , ShelleyBasedEra(..)
    , lovelaceToValue
    , valueFromList
    )

import Tokenomia.CardanoApi.Fees
    ( HasDatumHash(..)
    , calculateDefaultMinimumUTxOFromAssetId
    , calculateDefaultMinimumUTxOFromValue
    , utxoEntrySize
    )


tests :: TestTree
tests = testGroup "CardanoApi.Fees" [ unitTests ]

assertUTxOEntrySize :: [(AssetId, Quantity)]-> HasDatumHash -> Integer -> Assertion
assertUTxOEntrySize value hasDatumHash n =
    utxoEntrySize (valueFromList value) hasDatumHash @?= n

assertCalculateDefaultMinimumUTxOFromValue :: [(AssetId, Quantity)] -> HasDatumHash -> Integer -> Assertion
assertCalculateDefaultMinimumUTxOFromValue value hasDatumHash n =
    calculateDefaultMinimumUTxOFromValue ShelleyBasedEraAlonzo (valueFromList value) hasDatumHash
        @?= Just (lovelaceToValue (Lovelace n))

assertCalculateDefaultMinimumUTxOFromAssetId :: AssetId -> Integer -> Assertion
assertCalculateDefaultMinimumUTxOFromAssetId assetId n =
    calculateDefaultMinimumUTxOFromAssetId ShelleyBasedEraAlonzo assetId @?= Just n

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "utxoEntrySize"
        [ testCase "NoDatumHash (i)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1) ]
            NoDatumHash
            38
        , testCase "NoDatumHash (ii)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1) ]
            NoDatumHash
            39
        , testCase "NoDatumHash (iii)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "2"), 2)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "3"), 3)
            ]
            NoDatumHash
            42
        , testCase "NoDatumHash (iv)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName ""), 2)
            ]
            NoDatumHash
            43
        , testCase "NoDatumHash (v)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName "2"), 2)
            ]
            NoDatumHash
            44
        , testCase "WithDatumHash (i)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1) ]
            WithDatumHash
            48
        , testCase "WithDatumHash (ii)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "32 32 32 32 32 32 32 32 32 32 32"), 1)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "2 32 32 32 32 32 32 32 32 32 323"), 2)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName " 32 32 32 32 32 32 32 32 32 3232"), 3)
            ]
            WithDatumHash
            63
        , testCase "WithDatumHash (iii)" $ assertUTxOEntrySize
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName ""), 2)
            ]
            WithDatumHash
            53
        ]
    , testGroup "calculateDefaultMinimumUTxOFromValue"
        [ testCase "NoDatumHash (i)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1) ]
            NoDatumHash
            1310316
        , testCase "NoDatumHash (ii)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1) ]
            NoDatumHash
            1344798
        , testCase "NoDatumHash (iii)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "2"), 2)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "3"), 3)
            ]
            NoDatumHash
            1448244
        , testCase "NoDatumHash (iv)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName ""), 2)
            ]
            NoDatumHash
            1482726
        , testCase "NoDatumHash (v)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName "2"), 2)
            ]
            NoDatumHash
            1517208
        , testCase "WithDatumHash (i)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1) ]
            WithDatumHash
            1655136
        , testCase "WithDatumHash (ii)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "32 32 32 32 32 32 32 32 32 32 32"), 1)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "2 32 32 32 32 32 32 32 32 32 323"), 2)
            , (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName " 32 32 32 32 32 32 32 32 32 3232"), 3)
            ]
            WithDatumHash
            2172366
        , testCase "WithDatumHash (iii)" $ assertCalculateDefaultMinimumUTxOFromValue
            [ (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""), 1)
            , (AssetId (PolicyId "3ccd653511eec65bbd30c3489f53471b017c829bd97d3a2ae81fb818") (AssetName ""), 2)
            ]
            WithDatumHash
            1827546
        ]
    , testGroup "calculateDefaultMinimumUTxOFromAssetId"
        [ testCase "NoDatumHash (i)" $ assertCalculateDefaultMinimumUTxOFromAssetId
            (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName ""))
            1310316
        , testCase "NoDatumHash (ii)" $ assertCalculateDefaultMinimumUTxOFromAssetId
            (AssetId (PolicyId "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e") (AssetName "1"))
            1344798
        ]
    ]
