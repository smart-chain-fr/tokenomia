module Spec.Tokenomia.CardanoApi.FromPlutus.Value
    ( tests
    ) where

import Data.Either                                     ( isRight )

import Ledger.Ada                                      ( adaSymbol )
import Plutus.V1.Ledger.Value                          ( CurrencySymbol(..) )

import Test.Tasty                                      ( TestTree, testGroup )
import Test.Tasty.QuickCheck                           ( shrink, testProperty )

import Tokenomia.Common.Arbitrary.AssetClass           ()
import Tokenomia.Common.Arbitrary.Builtins             ()
import Tokenomia.Common.Arbitrary.Modifiers            ( Restricted(..) )
import Tokenomia.Common.Arbitrary.Value                ()

import Tokenomia.CardanoApi.FromPlutus.Value
    ( assetClassAsAssetId
    , currencySymbolAsPolicyId
    , fromPlutusValue
    , tokenNameAsAssetName
    )


tests :: TestTree
tests = testGroup "CardanoApi.FromPlutus.Value" [ properties ]

validCurrencySymbolAsPolicyId :: CurrencySymbol -> Bool
validCurrencySymbolAsPolicyId x =
    x == adaSymbol || isRight (currencySymbolAsPolicyId x)

propertiesCurrencySymbolAsPolicyId :: [TestTree]
propertiesCurrencySymbolAsPolicyId =
    [ testProperty "currencySymbolAsPolicyId on arbitrary" $
            validCurrencySymbolAsPolicyId . getRestricted
    , testProperty "currencySymbolAsPolicyId on shrinks" $
            \x -> and $ validCurrencySymbolAsPolicyId . getRestricted <$> shrink x
    ]

propertiesTokenNameAsAssetName :: [TestTree]
propertiesTokenNameAsAssetName =
    [ testProperty "tokenNameAsAssetName on arbitrary" $
            isRight . tokenNameAsAssetName . getRestricted
    , testProperty "tokenNameAsAssetName on shrinks" $
            \x -> and $ isRight . tokenNameAsAssetName . getRestricted <$> shrink x
    ]

propertiesAssetClassAsAssetId :: [TestTree]
propertiesAssetClassAsAssetId =
    [ testProperty "assetClassAsAssetId on arbitrary" $
            isRight . assetClassAsAssetId . getRestricted
    , testProperty "assetClassAsAssetId on shrinks" $
            \x -> and $ isRight . assetClassAsAssetId . getRestricted <$> shrink x
    ]

propertiesFromPlutusValue :: [TestTree]
propertiesFromPlutusValue =
    [ testProperty "fromPlutusValue on arbitrary" $
            isRight . fromPlutusValue . getRestricted
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "currencySymbolAsPolicyId" propertiesCurrencySymbolAsPolicyId
    , testGroup "tokenNameAsAssetName" propertiesTokenNameAsAssetName
    , testGroup "assetClassAsAssetId" propertiesAssetClassAsAssetId
    , testGroup "fromPlutusValue" propertiesFromPlutusValue
    ]
