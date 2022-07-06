{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Tokenomia.ICO.Funds.Exchange.Plan (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Debug.Trace
import Spec.Tokenomia.ICO.Funds.Exchange.GenInputs
import Tokenomia.ICO.Balanceable
import Tokenomia.ICO.Funds.Exchange.Command
import Tokenomia.ICO.Funds.Exchange.Plan

tests :: TestTree
tests = testGroup "ICO" [properties]

properties :: TestTree
properties =
  testGroup
    "Planning Token Exchange"
    [ QC.testProperty "Plan is Balanced with ADAs => ∑ adas Inputs  == ∑ adas collected + ∑ (exchange output * minimumRequiredAdaPerUtxo) + fees + ∑ adas refunds" $
        \settings feesMaybe tokensMaybe funds ->
          let (state, plan) = mkPlan' settings constMinimumUTxOAdaRequired feesMaybe tokensMaybe funds
           in traceIfFalse (showAdaBalanceDetails state plan) (adaBalance plan == 0)
    , QC.testProperty "Plan is Balanced with tokens under exchange" $
        \settings feesMaybe tokensMaybe funds ->
          let (state, plan) = mkPlan' settings constMinimumUTxOAdaRequired feesMaybe tokensMaybe funds
           in traceIfFalse (showTokenBalanceDetails state plan) (tokenBalance plan == 0)
    ]

showAdaBalanceDetails :: State -> Plan Command -> String
showAdaBalanceDetails state plan@Plan {..} =
  "\n|| Test Failed |"
    <> "\n|  Plan Balance = "
    <> show (adaBalance plan)
    <> "\n|  Commands Balance = "
    <> show (adaBalance commands)
    <> "\n|  Fees  = "
    <> show (adaBalance feesMaybe)
    <> "\n|  ioOnTokenAddress Balance = "
    <> show (adaBalance ioOnTokenAddress)
    <> "\n|  ioOnTokenAddress  = "
    <> show ioOnTokenAddress
    <> "\n|  "
    <> show state

showTokenBalanceDetails :: State -> Plan Command -> String
showTokenBalanceDetails state plan@Plan {..} =
  "\n|| Test Failed |"
    <> "\n|  Plan Balance = "
    <> show (tokenBalance plan)
    <> "\n|  Commands Balance = "
    <> show (tokenBalance commands)
    <> "\n|  ioOnTokenAddress Balance = "
    <> show (tokenBalance ioOnTokenAddress)
    <> "\n|  ioOnTokenAddress  = "
    <> show ioOnTokenAddress
    <> "\n|  "
    <> show state

traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True
