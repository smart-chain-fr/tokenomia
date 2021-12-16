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

module Spec.Tokenomia.ICO.Funds.Reception.Investor.Plan (tests) where

import Test.Tasty 
import qualified Test.Tasty.QuickCheck as QC



import Tokenomia.ICO.Funds.Reception.Investor.Plan
import Tokenomia.ICO.Funds.Reception.Investor.Command
import           Plutus.V1.Ledger.Interval as I
import           Tokenomia.ICO.Funds.Reception.Investor.Plan.Settings
import Spec.Tokenomia.ICO.Funds.Reception.Investor.GenInputs ()
import Debug.Trace
import           Data.Set.Ordered 
import Data.Monoid
import Data.Coerce



tests :: TestTree
tests = testGroup "UTxOs" [properties]


properties :: TestTree
properties = testGroup "Planning Investor Funds Validation" 
    [ QC.testProperty "Funds out of time range will be refund with an the appropriate reason given" $
        \settings investorAddressState -> 
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState  
            in traceIfFalse (showDetails state plan ) 
                (coerce $ foldMap All (refundWhenNotBelongToRoundTimeRange settings <$> toAscList commands) ),
    QC.testProperty "Funds below minimum funds given are ignored" $
        \settings investorAddressState -> 
            let (state,plan@InvestorPlan{..}) = mkPlan' settings investorAddressState  
            in traceIfFalse (showDetails state plan ) 
                (coerce $ foldMap All (isAboveMinimumAdaRequired settings <$> toAscList commands) )
    ]


isAboveMinimumAdaRequired :: PlanSettings -> Command -> Bool
isAboveMinimumAdaRequired Settings {..} c = getAdas c >= minimumAdaPerFund

refundWhenNotBelongToRoundTimeRange :: PlanSettings -> Command -> Bool
refundWhenNotBelongToRoundTimeRange Settings {..} Refund {reason = TransactionOutofRoundTimeRange,..} 
    = not $ I.member receivedAt timeRange
refundWhenNotBelongToRoundTimeRange Settings {..} c 
    = I.member (receivedAt c) timeRange


showDetails ::  State -> InvestorPlan -> String 
showDetails  state plan 
    =  "\n|| Test Failed |" 
    <> "\n|  " <> show plan
    <> "\n|  " <> show state 



traceIfFalse :: String -> Bool -> Bool
traceIfFalse msg False = trace msg False
traceIfFalse _ True = True