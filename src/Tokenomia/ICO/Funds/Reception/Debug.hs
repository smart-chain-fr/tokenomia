{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.Debug
    ( getStateAndPlans
    , displayStateAndPlans
    , showInvestorPlans
    ) where

import Prelude hiding (round,print)
import           Control.Monad.Reader

import Data.List (intersperse)

import           Tokenomia.Common.Shell.Console (printLn)

import Data.Foldable
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State
import           Tokenomia.ICO.Funds.Reception.Investor.Plan (InvestorPlan (..),mkPlan)
import           Data.Set.Ordered hiding (null)
import           Tokenomia.ICO.RoundSettings
import qualified Data.List.NonEmpty as NEL
import           Tokenomia.ICO.Funds.Reception.Investor.Command as Plan
import Data.Coerce
import           Tokenomia.ICO.Funds.Reception.Investor.Plan.Settings

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


showInvestorPlans
    :: ( MonadIO m)
    => NEL.NonEmpty InvestorPlan
    ->  m (NEL.NonEmpty InvestorPlan)
showInvestorPlans investorplans = do
    let funds = toAscList . unbiased $ fold (toBiasR . commands  <$> investorplans)
        nbfunds = length funds
        nbIgnored = nbfunds - nbRefund - nbSendOnExchangeAddressWithPartialRefund - nbSendOnExchangeAddess   
        nbRefund = length $ Prelude.filter isRefund funds
        nbSendOnExchangeAddressWithPartialRefund = length $ Prelude.filter isSendOnExchangeAddressWithPartialRefund funds
        nbSendOnExchangeAddess = length $ Prelude.filter isSendOnExchangeAddress funds
    printLn $ "\n--------------------------------------"
           <> "\n| Batch Plan"
           <> "\n  - Investors                                     : " <> (show.length) investorplans
           <> "\n  - Funds                                         : " <> show nbfunds
           <> "\n  - To send on Exchange Addr                      : " <> show nbSendOnExchangeAddess
           <> "\n  - To send on Exchange Addr with partial refund  : " <> show nbSendOnExchangeAddressWithPartialRefund
           <> "\n  - To send on Exchange Addr with partial refund  : " <> show nbSendOnExchangeAddressWithPartialRefund
           <> "\n  - Ignored                                       : " <> show nbIgnored
           <> "\n----------------------------------------"
        --    <> "\n" <> fold (intersperse "\n" (show <$> x))
    return investorplans


toBiasR :: a -> Bias R a
toBiasR = coerce