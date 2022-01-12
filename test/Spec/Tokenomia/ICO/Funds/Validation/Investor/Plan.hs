{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Tokenomia.ICO.Funds.Validation.Investor.Plan (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC


import Ledger.Ada
import Tokenomia.ICO.Funds.Validation.Investor.Plan
import Tokenomia.ICO.Funds.Validation.Investor.Command as C
import           Plutus.V1.Ledger.Interval as I
import           Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings
import Spec.Tokenomia.ICO.Funds.Validation.Investor.GenInputs ()
import Debug.Trace
import           Data.Set.Ordered
import Data.Monoid
import Data.Coerce
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types as S





tests :: TestTree
tests = testGroup "ICO" [properties]


properties :: TestTree
properties = testGroup "Planning Investor Funds Validation"
    [ QC.testProperty "Funds out of time range will be refund" $
        \settings investorAddressState ->
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState
            in traceIfFalse (showDetails (S.allReceivedFunds investorAddressState) state plan )
                (coerce $ foldMap All (refundWhenNotBelongToRoundTimeRange settings <$> toAscList commands) ),
    QC.testProperty "Funds below minimum funds given and funds with tokens are ignored" $
        \settings investorAddressState ->
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState
            in traceIfFalse (showDetails (S.allReceivedFunds investorAddressState) state plan )
                (fundsWithNativeTokenAndBelowMinimumAreIgnored settings (S.allReceivedFunds investorAddressState) commands),
    QC.testProperty "Refunds and Amount sent on Exchange Address are above the minimum funds given" $
        \settings investorAddressState ->
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState
            in traceIfFalse (showDetails (S.allReceivedFunds investorAddressState) state plan )
                (coerce $ foldMap All (adasAbove (minimumAdaPerFund settings) <$> toAscList commands)),
    QC.testProperty "Funds will be refund when address is saturated" $
        \settings investorAddressState ->
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState
            in traceIfFalse (showDetails (S.allReceivedFunds investorAddressState) state plan )
                (refundWhenAddressSaturated settings (S.allReceivedFunds investorAddressState) (S.volumes investorAddressState) commands),
    QC.testProperty "Plan is Balanced" $
        \settings investorAddressState ->
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState
            in traceIfFalse (showDetails (S.allReceivedFunds investorAddressState) state plan )
                (planIsBalanced (S.allReceivedFunds investorAddressState) commands)
    ]

planIsBalanced :: OSet ReceivedFunds -> OSet Command -> Bool
planIsBalanced allReceivedFunds commands = 
    foldMap S.getAdas allReceivedFunds 
        == foldMap getAdasToRefundBecauseOutOfRange commands
         + foldMap getAdasToRefundBecauseAdresseSaturated commands
         + foldMap getAdasSendOnExchange commands
         + foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands)

fundsWithNativeTokenAndBelowMinimumAreIgnored :: PlanSettings -> OSet ReceivedFunds -> OSet Command -> Bool
fundsWithNativeTokenAndBelowMinimumAreIgnored Settings {..} allReceivedFunds commands 
 | size allReceivedFunds /= size commands  =
    let ignoredFunds = getIgnoredFunds allReceivedFunds commands
    in any (\funds -> isNativeTokenFund funds || S.getAdas funds < minimumAdaPerFund) ignoredFunds
  | otherwise = True  


adasAbove:: Ada -> Command -> Bool    
adasAbove adas Refund {..} = refundAmount >= adas
adasAbove adas SendOnExchangeAddressWithPartialRefund {..} = adasToSendOnExchange >= adas && refundAmount >= adas
adasAbove adas SendOnExchangeAddress  {..} = adasToSendOnExchange >= adas

refundWhenAddressSaturated :: PlanSettings -> OSet ReceivedFunds -> AddressVolumes -> OSet Command  -> Bool
refundWhenAddressSaturated Settings {..} allReceivedFunds AddressVolumes {..} commands
    | received > maximumAdaPerAddress  =
        received
            - sent
            - foldMap getAdasToRefundBecauseOutOfRange commands
            - foldMap getAdasSendOnExchange commands
            - foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands)
        == foldMap getAdasToRefundBecauseAdresseSaturated commands
    | otherwise = 0 == foldMap getAdasToRefundBecauseAdresseSaturated commands


refundWhenNotBelongToRoundTimeRange :: PlanSettings -> Command -> Bool
refundWhenNotBelongToRoundTimeRange Settings {..} Refund {reason = TransactionOutofRoundTimeRange,..}
    = not $ I.member receivedAt timeRange
refundWhenNotBelongToRoundTimeRange Settings {..} c
    = I.member (C.receivedAt c) timeRange


showDetails ::  OSet ReceivedFunds -> State -> InvestorPlan -> String
showDetails allReceivedFunds state plan@InvestorPlan {..}
    =  "\n|| Test Failed |"
    <> "\n|  " <> show plan
    <> "\n|  " <> show state
    <> "\n|  funds amount = " <> show (foldMap S.getAdas allReceivedFunds)
    <> "\n|  amount to send for exchange = " <> show (foldMap getAdasSendOnExchange commands)
    <> "\n|  saturated funds amount = " <> show (foldMap getAdasToRefundBecauseAdresseSaturated commands)
    <> "\n|  time out of range funds amount = " <> show (foldMap getAdasToRefundBecauseOutOfRange commands)
    <> "\n|  funds ignored amount = " <> show (foldMap S.getAdas (getIgnoredFunds allReceivedFunds commands))
    <> "\n|  funds ignored = " <> show (getIgnoredFunds allReceivedFunds commands)



traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True