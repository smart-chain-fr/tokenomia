{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Spec.Tokenomia.ICO.Funds.Validation.CardanoCLI.GenInputs
    ( ) where


import           Prelude hiding ((+), print)

import Ledger ( TxOutRef (..), Slot (..))
import           Plutus.V1.Ledger.Ada
import Test.QuickCheck
import           System.Random
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Test.QuickCheck.Instances ()
import           Tokenomia.Common.Hash
import           Tokenomia.Wallet.UTxO
import  Data.Set.NonEmpty as NES
import  Tokenomia.ICO.Funds.Validation.CardanoCLI.Command


instance (Ord a,Arbitrary a) => Arbitrary (NESet a) where
  arbitrary = NES.fromList <$> arbitrary


instance Arbitrary Command where
  arbitrary = do 
     minimumAdaRequiredPerUtxo <- choose (lovelaceOf 5 , lovelaceOf 10) -- prequesite funds are always above a given minimum of adas
     oneof 
       [ genSendOnExchangeAddressAndPartiallyRefund minimumAdaRequiredPerUtxo
       , genSendOnExchangeAddressAndPartiallyMoveToNextRound minimumAdaRequiredPerUtxo
       , genSendOnExchangeAddress minimumAdaRequiredPerUtxo
       , genRefund minimumAdaRequiredPerUtxo
       , genMoveToNextRound minimumAdaRequiredPerUtxo]

genFees :: Gen Ada
genFees = choose (lovelaceOf 1 , lovelaceOf 3)

genSendOnExchangeAddressAndPartiallyRefund :: Ada -> Gen Command
genSendOnExchangeAddressAndPartiallyRefund minimumAdaRequiredPerUtxo = do 
  sourceAmount <- choose (minimumAdaRequiredPerUtxo * lovelaceOf 2 , lovelaceOf 30)
  adasToRefundAmount <- choose (minimumAdaRequiredPerUtxo  , sourceAmount)
  let adasToSendOnExchangeAmount = sourceAmount - adasToRefundAmount
  
  SendOnExchangeAddressAndPartiallyRefund
    <$> genSourceWithAda sourceAmount
    <*> pure "fakeExchangeAddress"
    <*> pure "fakeRefundAddress"
    <*> pure adasToSendOnExchangeAmount
    <*> pure "mydatumFilePath"
    <*> pure adasToRefundAmount
    <*> genSlot

genSendOnExchangeAddressAndPartiallyMoveToNextRound :: Ada -> Gen Command
genSendOnExchangeAddressAndPartiallyMoveToNextRound minimumAdaRequiredPerUtxo = do 
  sourceAmount <- choose (minimumAdaRequiredPerUtxo * lovelaceOf 2 , lovelaceOf 30)
  adasToRefundAmount <- choose (minimumAdaRequiredPerUtxo  , sourceAmount)
  let adasToSendOnExchangeAmount = sourceAmount - adasToRefundAmount
  
  SendOnExchangeAddressAndPartiallyMoveToNextRound
    <$> genSourceWithAda sourceAmount
    <*> pure "fakeExchangeAddress"
    <*> pure "fakeNextRoundExchangeAddress"
    <*> pure adasToSendOnExchangeAmount
    <*> pure "mydatumFilePath"
    <*> pure adasToRefundAmount
    <*> genSlot



genSendOnExchangeAddress :: Ada -> Gen Command
genSendOnExchangeAddress minimumAdaRequiredPerUtxo = do 
  sourceAmount <- choose (minimumAdaRequiredPerUtxo , lovelaceOf 30)
  
  SendOnExchangeAddress
    <$> genSourceWithAda sourceAmount
    <*> pure "fakeExchangeAddress"
    <*> pure sourceAmount
    <*> pure "mydatumFilePath"
    <*> genSlot

genRefund :: Ada -> Gen Command
genRefund minimumAdaRequiredPerUtxo = do 
  sourceAmount <- choose (minimumAdaRequiredPerUtxo , lovelaceOf 30)
  Refund
    <$> genSourceWithAda sourceAmount
    <*> pure "fakeAddressRefund"
    <*> pure sourceAmount
    <*> genSlot

genMoveToNextRound :: Ada -> Gen Command
genMoveToNextRound minimumAdaRequiredPerUtxo = do 
  sourceAmount <- choose (minimumAdaRequiredPerUtxo , lovelaceOf 30)
  MoveToNextRound
    <$> genSourceWithAda sourceAmount
    <*> pure "fakeNextRoundExchangeAddress"
    <*> pure "mydatumFilePath"
    <*> pure sourceAmount
    <*> genSlot

instance Arbitrary Ada where
  arbitrary = genFees

deriving instance Random Ada



genSourceWithAda :: Ada ->  Gen WalletUTxO
genSourceWithAda adaAmount =
  WalletUTxO
    <$> constChildAddressRef
    <*> (UTxO
        <$> genTxOutRef
        <*> (pure . toValue) adaAmount
        <*> constNoDatum)



genSlot :: Gen Slot
genSlot = Slot <$> choose (0,10000)


constNoDatum :: Gen (Maybe Hash)
constNoDatum = pure Nothing

genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef "42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2" <$> choose (1,10000)

constChildAddressRef :: Gen ChildAddressRef
constChildAddressRef = pure $ ChildAddressRef "aWalletName" 0
