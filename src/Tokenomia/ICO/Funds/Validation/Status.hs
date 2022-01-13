{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Validation.Status
    ( displayStateAndPlans
    , displayInvestorPlans
    , getStateAndPlans
    ) where

import Prelude hiding (round,print)
import           Control.Monad.Reader

import Data.List (intersperse)

import           Tokenomia.Common.Shell.Console (printLn)
import           Control.Monad.Except
import Data.Foldable
import           Tokenomia.ICO.Funds.Validation.ChildAddress.State 

import           Tokenomia.ICO.Funds.Validation.Investor.Plan (InvestorPlan (..),mkPlan)
import           Data.Set.Ordered hiding (null)
import           Tokenomia.ICO.Round.Settings
import qualified Data.List.NonEmpty as NEL
import           Tokenomia.ICO.Funds.Validation.Investor.Command as Plan
import Data.Coerce
import           Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings
import           Tokenomia.Common.Error

getStateAndPlans
    :: RoundSettings
    -> NEL.NonEmpty WhiteListedInvestorState
    -> NEL.NonEmpty (WhiteListedInvestorState, InvestorPlan)
getStateAndPlans round whiteListedInvestorStates
    =  (\a -> (a,mkPlan (mkPlanSettings round) a)) <$> whiteListedInvestorStates

displayStateAndPlans
    :: ( MonadIO m)
    => NEL.NonEmpty (WhiteListedInvestorState, InvestorPlan)
    ->  m (NEL.NonEmpty (WhiteListedInvestorState, InvestorPlan))
displayStateAndPlans statesAndplans = do
    sequence_ $ uncurry displayStateAndPlan  <$> statesAndplans
    return statesAndplans

displayStateAndPlan
    :: ( MonadIO m)
    => WhiteListedInvestorState
    ->  InvestorPlan
    -> m ()
displayStateAndPlan
    WhiteListedInvestorState {allReceivedFunds,volumes}
    InvestorPlan {investorRef,commands} | not (null allReceivedFunds)
    = printLn
        $  "\n--------------------------------------"
        <> "\n|| Investor ||\n" <> show investorRef
        <> "\n|| Address Status ||\n"
        <> "\n||| Volumes "
        <> "\n" <> show volumes
        <> "\n||| Funds Received : " <> (show . length) allReceivedFunds
        <> "\n" <> fold (intersperse "\n" (show <$> toAscList allReceivedFunds))
        <> "\n||| Plan \n"
        <> "\n" <> fold (intersperse "\n" (show <$> toAscList commands))
        <> "\n----------------------------------------"
displayStateAndPlan _ InvestorPlan {..}
    = printLn
        $  "\n----------------------------------------"
        <> "\n" <> show investorRef
        <> "\n no Funds Received"
        <> "\n----------------------------------------"

displayState
    :: ( MonadIO m)
    => WhiteListedInvestorState
    -> m ()
displayState
    WhiteListedInvestorState {allReceivedFunds,volumes}
    = printLn
        $  "\n--------------------------------------"
        <> "\n|| Address Status ||\n"
        <> "\n||| Volumes "
        <> "\n" <> show volumes
        <> "\n||| Funds Received : " <> (show . length) allReceivedFunds
        <> "\n" <> fold (intersperse "\n" (show <$> toAscList allReceivedFunds))
        <> "\n----------------------------------------"


displayInvestorPlans
    :: ( MonadIO m 
       , MonadError  TokenomiaError m)
    => NEL.NonEmpty InvestorPlan
    ->  m (NEL.NonEmpty InvestorPlan)
displayInvestorPlans investorplans = do
    let funds = toAscList . unbiased $ fold (toBiasR . commands  <$> investorplans)
        nbfunds = length funds
        nbIgnored = nbfunds - nbReject - nbSendOnExchangeAddressAndPartiallyReject - nbSendOnExchangeAddess
        nbReject = length $ Prelude.filter isReject funds
        nbSendOnExchangeAddressAndPartiallyReject = length $ Prelude.filter isSendOnExchangeAddressAndPartiallyReject funds
        nbSendOnExchangeAddess = length $ Prelude.filter isSendOnExchangeAddress funds
    printLn $ "\n--------------------------------------"
           <> "\n| Batch Plan"
           <> "\n  - Investors                                   : " <> (show.length) investorplans
           <> "\n  - Funds                                       : " <> show nbfunds
           <> "\n  - Send on exchange addr                       : " <> show nbSendOnExchangeAddess
           <> "\n  - Send on exchange addr and partially reject  : " <> show nbSendOnExchangeAddressAndPartiallyReject
           <> "\n  - Reject                                      : " <> show nbReject
           <> "\n  - Ignored                                     : " <> show nbIgnored
           <> "\n----------------------------------------"
        --    <> show investorplans
    when (nbSendOnExchangeAddressAndPartiallyReject > 0) $ 
        mapM_
        (\command -> do 
            printLn $ show command
            state <- fetchWhiteListedFunds (Plan.investorRef command)
            displayState state)
        (Prelude.filter isSendOnExchangeAddressAndPartiallyReject funds)
        
    when (nbReject > 0) $ 
        mapM_
        (\command -> do 
            printLn $ show command
            state <- fetchWhiteListedFunds (Plan.investorRef command)
            displayState state)
        (Prelude.filter isReject funds)

    return investorplans


toBiasR :: a -> Bias R a
toBiasR = coerce