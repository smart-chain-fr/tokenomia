{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Tokenomia.ICO.Funds.Exchange.GenInputs (
  genFees,
  genExchangeToken,
  genPlanSetings,
  genAuthentifiedFunds,
  constMinimumUTxOAdaRequired,
) where

import PlutusTx.Prelude (AdditiveSemigroup ((+)))
import Prelude hiding (print, (+), (-))

import Data.Set.NonEmpty as NES
import Ledger (Slot (..), TxOutRef (..))
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Value
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Tokenomia.Common.Address
import Tokenomia.Common.Hash
import Tokenomia.Common.Token hiding (assetClass)
import Tokenomia.ICO.Funds.Exchange.Plan.Settings
import Tokenomia.ICO.Funds.Exchange.ReceivedFunds
import Tokenomia.ICO.Funds.Exchange.Tokens
import Tokenomia.ICO.Round.Settings
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.UTxO
import Tokenomia.Wallet.WalletUTxO

instance (Ord a, Arbitrary a) => Arbitrary (NESet a) where
  arbitrary = NES.fromList <$> arbitrary

instance Arbitrary PlanSettings where
  arbitrary = genPlanSetings

genPlanSetings :: Gen PlanSettings
genPlanSetings =
  let ratio = choose @Double (1, 3)
   in pure (Settings constTokenAssetClass)
        <*> (RatePerLovelace <$> ratio)

instance Arbitrary Ada where
  arbitrary = genFees

deriving instance Random Ada

genFees :: Gen Ada
genFees = choose (lovelaceOf 1, lovelaceOf 3)

instance Arbitrary ExchangeToken where
  arbitrary = genExchangeToken

genExchangeToken :: Gen ExchangeToken
genExchangeToken = do
  tokenAmount <- choose @Integer (10, 100)
  ExchangeToken
    <$> genSourceWithToken tokenAmount
    <*> ( pure (Token constTokenAssetClass)
            <*> pure tokenAmount
            <*> pure constMinimumUTxOAdaRequired
        )

constMinimumUTxOAdaRequired :: Ada
constMinimumUTxOAdaRequired = 2

genSourceWithToken :: Integer -> Gen WalletUTxO
genSourceWithToken tokenAmount =
  WalletUTxO
    <$> constChildAddressRef
    <*> ( UTxO
            <$> genTxOutRef
            <*> pure (buildValueWithTokenAndMinimumAda tokenAmount)
            <*> constNoDatum
        )

buildValueWithTokenAndMinimumAda :: Integer -> Value
buildValueWithTokenAndMinimumAda tokenAmount =
  toValue constMinimumUTxOAdaRequired + assetClassValue constTokenAssetClass tokenAmount

instance Arbitrary AuthentifiedFunds where
  arbitrary = genAuthentifiedFunds

genAuthentifiedFunds :: Gen AuthentifiedFunds
genAuthentifiedFunds = do
  adasGiven <- choose (lovelaceOf 5, lovelaceOf 15)
  AuthentifiedFunds
    <$> genSourceWithAda adasGiven
    <*> pure adasGiven
    <*> genSlot
    <*> constIndex
    <*> constAddress

constIndex :: Gen ChildAddressIndex
constIndex = pure 1234

constAddress :: Gen Address
constAddress = pure $ Address "fakeAddress"

genSourceWithAda :: Ada -> Gen WalletUTxO
genSourceWithAda adaAmount =
  WalletUTxO
    <$> constChildAddressRef
    <*> ( UTxO
            <$> genTxOutRef
            <*> (pure . toValue) adaAmount
            <*> constNoDatum
        )

genSlot :: Gen Slot
genSlot = Slot <$> choose (0, 10000)

constNoDatum :: Gen (Maybe Hash)
constNoDatum = pure Nothing

genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef "42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2" <$> choose (1, 10000)

constTokenAssetClass :: AssetClass
constTokenAssetClass = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "aTokenName"

constChildAddressRef :: Gen ChildAddressRef
constChildAddressRef = pure $ ChildAddressRef "aWalletName" 0
