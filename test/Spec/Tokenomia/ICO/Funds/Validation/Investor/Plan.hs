{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Tokenomia.ICO.Funds.Validation.Investor.Plan (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Data.Coerce
import Data.Monoid
import Data.Set.Ordered
import Debug.Trace
import Ledger.Ada
import Plutus.V1.Ledger.Interval as I
import Spec.Tokenomia.ICO.Funds.Validation.Investor.GenInputs ()
import Tokenomia.ICO.Funds.Validation.ChildAddress.Types as S
import Tokenomia.ICO.Funds.Validation.Investor.Command as C
import Tokenomia.ICO.Funds.Validation.Investor.Plan
import Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings

tests :: TestTree
tests = testGroup "ICO" [properties]

properties :: TestTree
properties =
  testGroup
    "Planning Investor Funds Validation"
    [ QC.testProperty "Funds out of time range will be rejected" $
        \settings investorAddressState ->
          let (state, plan@InvestorPlan {..}) = mkPlan' settings investorAddressState
           in traceIfFalse
                (showDetails (S.allReceivedFunds investorAddressState) state plan)
                (coerce $ foldMap All (rejectWhenNotBelongToRoundTimeRange settings <$> toAscList commands))
    , QC.testProperty "Funds below minimum funds given and funds with tokens are ignored" $
        \settings investorAddressState ->
          let (state, plan@InvestorPlan {..}) = mkPlan' settings investorAddressState
           in traceIfFalse
                (showDetails (S.allReceivedFunds investorAddressState) state plan)
                (fundsWithNativeTokenAndBelowMinimumAreIgnored settings (S.allReceivedFunds investorAddressState) commands)
    , QC.testProperty "Rejects and Amounts sent on Exchange Address are above the minimum funds required" $
        \settings investorAddressState ->
          let (state, plan@InvestorPlan {..}) = mkPlan' settings investorAddressState
           in traceIfFalse
                (showDetails (S.allReceivedFunds investorAddressState) state plan)
                (coerce $ foldMap All (adasAbove (minimumAdaPerFund settings) <$> toAscList commands))
    , QC.testProperty "Funds will be rejected when address is saturated" $
        \settings investorAddressState ->
          let (state, plan@InvestorPlan {..}) = mkPlan' settings investorAddressState
           in traceIfFalse
                (showDetails (S.allReceivedFunds investorAddressState) state plan)
                (refundWhenAddressSaturated settings (S.allReceivedFunds investorAddressState) (S.volumes investorAddressState) commands)
    , QC.testProperty "Plan is Balanced" $
        \settings investorAddressState ->
          let (state, plan@InvestorPlan {..}) = mkPlan' settings investorAddressState
           in traceIfFalse
                (showDetails (S.allReceivedFunds investorAddressState) state plan)
                (planIsBalanced (S.allReceivedFunds investorAddressState) commands)
    ]

planIsBalanced :: OSet ReceivedFunds -> OSet Command -> Bool
planIsBalanced allReceivedFunds commands =
  foldMap S.getAdas allReceivedFunds
    == foldMap getAdasToRejectBecauseOutOfRange commands
      + foldMap getAdasToRejectBecauseAdresseSaturated commands
      + foldMap getAdasSendOnExchange commands
      + foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands)

fundsWithNativeTokenAndBelowMinimumAreIgnored :: PlanSettings -> OSet ReceivedFunds -> OSet Command -> Bool
fundsWithNativeTokenAndBelowMinimumAreIgnored Settings {..} allReceivedFunds commands
  | size allReceivedFunds /= size commands =
    let ignoredFunds = getIgnoredFunds allReceivedFunds commands
     in any (\funds -> isNativeTokenFund funds || S.getAdas funds < minimumAdaPerFund) ignoredFunds
  | otherwise = True

adasAbove :: Ada -> Command -> Bool
adasAbove adas Reject {..} = amountToReject >= adas
adasAbove adas SendOnExchangeAddressWithPartialReject {..} = adasToSendOnExchange >= adas && amountToReject >= adas
adasAbove adas SendOnExchangeAddress {..} = adasToSendOnExchange >= adas

refundWhenAddressSaturated :: PlanSettings -> OSet ReceivedFunds -> AddressVolumes -> OSet Command -> Bool
refundWhenAddressSaturated Settings {..} allReceivedFunds AddressVolumes {..} commands
  | received > maximumAdaPerAddress =
    received
      - sent
      - foldMap getAdasToRejectBecauseOutOfRange commands
      - foldMap getAdasSendOnExchange commands
      - foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands)
      == foldMap getAdasToRejectBecauseAdresseSaturated commands
  | otherwise = 0 == foldMap getAdasToRejectBecauseAdresseSaturated commands

rejectWhenNotBelongToRoundTimeRange :: PlanSettings -> Command -> Bool
rejectWhenNotBelongToRoundTimeRange Settings {..} Reject {reason = TransactionOutofRoundTimeRange, ..} =
  not $ I.member receivedAt timeRange
rejectWhenNotBelongToRoundTimeRange Settings {..} c =
  I.member (C.receivedAt c) timeRange

showDetails :: OSet ReceivedFunds -> State -> InvestorPlan -> String
showDetails allReceivedFunds state plan@InvestorPlan {..} =
  "\n|| Test Failed |"
    <> "\n|  "
    <> show plan
    <> "\n|  "
    <> show state
    <> "\n|  funds amount = "
    <> show (foldMap S.getAdas allReceivedFunds)
    <> "\n|  amount to send for exchange = "
    <> show (foldMap getAdasSendOnExchange commands)
    <> "\n|  saturated funds amount = "
    <> show (foldMap getAdasToRejectBecauseAdresseSaturated commands)
    <> "\n|  time out of range funds amount = "
    <> show (foldMap getAdasToRejectBecauseOutOfRange commands)
    <> "\n|  funds ignored amount = "
    <> show (foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands))
    <> "\n|  funds ignored = "
    <> show (getIgnoredFunds allReceivedFunds commands)

traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True
