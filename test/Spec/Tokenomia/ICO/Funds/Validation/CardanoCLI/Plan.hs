{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Tokenomia.ICO.Funds.Validation.CardanoCLI.Command as C
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan

import Debug.Trace
import Spec.Tokenomia.ICO.Funds.Validation.Investor.GenInputs ()

import Spec.Tokenomia.ICO.Funds.Validation.CardanoCLI.GenInputs ()
import Tokenomia.ICO.Balanceable

tests :: TestTree
tests = testGroup "ICO" [properties]

properties :: TestTree
properties =
  testGroup
    "Planning Validated Funds"
    [ QC.testProperty "Plan is Balanced with given fees" $
        \fees unbalancedFees ->
          let (state, plan) = mkPlan' fees unbalancedFees
           in traceIfFalse (showAdaBalanceDetails state plan) (adaBalance plan == 0)
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
    <> "\n|  "
    <> show state

traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True
