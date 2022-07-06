{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Tokenomia.ICO.Funds.Validation.Investor.GenInputs () where

import PlutusTx.Prelude (AdditiveSemigroup ((+)))
import Prelude hiding (print, (+), (-))

import Ledger (Slot (..), TxOutRef (..))
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Interval
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.Common.Address

import Blockfrost.Types.Shared.Amount (Amount (..))
import Data.Coerce
import Data.Set.Ordered
import Money qualified
import Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings

instance Arbitrary WhiteListedInvestorState where
  arbitrary =
    ( pure (WhiteListedInvestorState constWhiteListedInvestorRef)
        <*> genAddressVolumes
        <*> arbitrary
    )
      `suchThat` volumesMatchesFundsReceived

instance (Ord a, Arbitrary a) => Arbitrary (OSet a) where
  arbitrary = fromList <$> arbitrary

instance Arbitrary PlanSettings where
  arbitrary = genPlanSettings

genPlanSettings :: Gen PlanSettings
genPlanSettings = do
  minAdas <- choose (lovelaceOf 5, lovelaceOf 15)
  maxAdas <- (minAdas +) <$> choose (lovelaceOf 5, lovelaceOf 20)
  minSlot <- Slot <$> choose (0, 10000)
  maxSlot <- (minSlot +) <$> (Slot <$> choose (0, 10000))
  pure (Settings (interval minSlot maxSlot))
    <*> pure minAdas
    <*> pure maxAdas

genAddressVolumes :: Gen AddressVolumes
genAddressVolumes = do
  adasSent <- choose (lovelaceOf 5, lovelaceOf 15)
  adasReceived <- (adasSent +) <$> choose (lovelaceOf 5, lovelaceOf 100)
  pure (AddressVolumes adasReceived adasSent)

deriving instance Random Ada

constWhiteListedInvestorRef :: WhiteListedInvestorRef
constWhiteListedInvestorRef =
  WhiteListedInvestorRef
    (Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8")
    ( IndexedAddress
        (Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8")
        (ChildAddressRef "awalletName" 0)
    )

instance Arbitrary ReceivedFunds where
  arbitrary = genReceivedFunds

genReceivedFunds :: Gen ReceivedFunds
genReceivedFunds = do
  slot <- genSlot
  ReceivedFunds
    <$> constTxOutRef slot
    <*> genFunds
    <*> genSlot

genFunds :: Gen Funds
genFunds =
  oneof
    [ pure (Left constNativeTokens)
    , Right <$> choose (lovelaceOf 5, lovelaceOf 10)
    ]

constNativeTokens :: NativeTokens
constNativeTokens =
  [ AssetAmount $
      Money.toSomeDiscrete
        ( 12 ::
            Money.Discrete'
              "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
              '(1, 1)
        )
  , AdaAmount 5
  ]

genSlot :: Gen Slot
genSlot = Slot <$> choose (0, 10000)

constTxOutRef :: Slot -> Gen TxOutRef -- use the Slot Number to make TxOutRef and Slot Number consisten
constTxOutRef slot =
  pure
    ( TxOutRef
        "42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2"
        (coerce slot)
    )
