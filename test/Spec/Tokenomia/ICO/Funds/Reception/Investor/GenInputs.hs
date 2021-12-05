{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Spec.Tokenomia.ICO.Funds.Reception.Investor.GenInputs
    ( ) where

import           Prelude hiding ((+),(-), print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)))

import Ledger ( TxOutRef (..), Slot (..))
import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Interval
import Test.QuickCheck
import           System.Random
import           Test.QuickCheck.Instances ()
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.Common.Address

import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import qualified Money
import           Blockfrost.Types.Shared.Amount ( Amount(..) )
import           Data.Set.Ordered
import           Tokenomia.ICO.Funds.Reception.Investor.Plan.Settings


instance Arbitrary WhiteListedInvestorState where
  arbitrary =
      WhiteListedInvestorState
       <$> pure constWhiteListedInvestorRef
       <*> genAddressVolumes
       <*> arbitrary


instance (Ord a,Arbitrary a) => Arbitrary (OSet a) where
  arbitrary = fromList <$> arbitrary


instance Arbitrary PlanSettings where
  arbitrary = genPlanSettings

genPlanSettings :: Gen PlanSettings
genPlanSettings = do
    minAdas <- choose (lovelaceOf 5 , lovelaceOf 15)
    maxAdas <- (minAdas + ) <$> choose (lovelaceOf 5 , lovelaceOf 100)
    minSlot <- Slot <$> choose (0,10000)
    maxSlot <- (minSlot + ) <$> (Slot <$> choose (0,10000))
    Settings
      <$> pure (interval minSlot maxSlot)
      <*> pure minAdas
      <*> pure maxAdas

genAddressVolumes :: Gen AddressVolumes
genAddressVolumes = do
  adasReceived <- choose (lovelaceOf 5 , lovelaceOf 15)
  adasSent <- (adasReceived + ) <$> choose (lovelaceOf 5 , lovelaceOf 1000)
  pure (AddressVolumes adasReceived) <*> pure adasSent

deriving instance Random Ada

constWhiteListedInvestorRef :: WhiteListedInvestorRef
constWhiteListedInvestorRef
  = WhiteListedInvestorRef
        (Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8")
        (IndexedAddress
          (Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8")
          (ChildAddressRef "awalletName" 0))

instance  Arbitrary ReceivedFunds where
  arbitrary = genReceivedFunds



genReceivedFunds :: Gen ReceivedFunds
genReceivedFunds =
  ReceivedFunds
    <$> genTxOutRef
    <*> genFunds
    <*> genSlot



genFunds :: Gen Funds
genFunds =
  oneof
    [ pure (Left constNativeTokens)
    , Right <$> choose (lovelaceOf 5 , lovelaceOf 15) ]


constNativeTokens :: NativeTokens
constNativeTokens =
     [AssetAmount
        $ Money.toSomeDiscrete
          (12 :: Money.Discrete'
                    "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                    '(1,1))]

genSlot :: Gen Slot
genSlot = Slot <$> choose (0,10000)


genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef "42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2" <$> choose (1,10000)
