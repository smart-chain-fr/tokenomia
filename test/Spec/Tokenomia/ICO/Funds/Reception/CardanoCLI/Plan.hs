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

module Spec.Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC



import Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan
import Tokenomia.ICO.Funds.Reception.CardanoCLI.Command as C

import Spec.Tokenomia.ICO.Funds.Reception.Investor.GenInputs ()
import Debug.Trace

import           Spec.Tokenomia.ICO.Funds.Reception.CardanoCLI.GenInputs ()
import           Tokenomia.ICO.Balanceable


tests :: TestTree
tests = testGroup "ICO" [properties]


properties :: TestTree
properties = testGroup "Planning Validated Funds"
    [ QC.testProperty "Plan is Balanced with given fees" $
        \fees unbalancedFees ->
            let (state,plan) = mkPlan' fees unbalancedFees
            in traceIfFalse (showAdaBalanceDetails state plan ) (adaBalance plan == 0)
    ]

showAdaBalanceDetails ::  State -> Plan Command -> String 
showAdaBalanceDetails  state plan@Plan{..} 
    =  "\n|| Test Failed |" 
    <> "\n|  Plan Balance = " <> show (adaBalance plan)
    <> "\n|  Commands Balance = " <> show (adaBalance commands )
    <> "\n|  Fees  = " <> show (adaBalance feesMaybe)
    <> "\n|  " <> show state 

traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True