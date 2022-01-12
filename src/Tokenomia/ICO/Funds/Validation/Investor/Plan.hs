{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Validation.Investor.Plan
    ( mkPlan
    , mkPlan'
    , State (..)
    , InvestorPlan (..)
    , getIgnoredFunds) where
import           Prelude hiding (round,print)

import           Data.Set.Ordered

import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Interval as I

import           Tokenomia.ICO.Funds.Validation.Investor.Command as C
import           Tokenomia.ICO.Funds.Validation.ChildAddress.State
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types hiding (getAdas)
import           Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings



data InvestorPlan = InvestorPlan {investorRef :: WhiteListedInvestorRef , commands :: OSet Command} deriving (Show,Eq)

mkPlan
    :: PlanSettings
    -> WhiteListedInvestorState
    ->  InvestorPlan
mkPlan a b = snd $ mkPlan' a b

mkPlan'
    :: PlanSettings
    -> WhiteListedInvestorState
    ->  (State,InvestorPlan)
mkPlan'  settings@Settings{..} WhiteListedInvestorState {..}
    = let s@State {commands} = foldr
                                transition
                                State {commands = empty
                                      , fundRange = I.interval minimumAdaPerFund maximumAdaPerAddress
                                      ,.. }
                                (reverse .toAscList $ allReceivedFunds)
    in (s,InvestorPlan
      { investorRef = investorRef
      , commands = commands})


getIgnoredFunds :: OSet ReceivedFunds  -> OSet Command -> [ReceivedFunds ]
getIgnoredFunds allReceivedFunds commands =
    Prelude.filter
      (\ReceivedFunds{txOutRef = txOutRefReceeivedFunds}
        -> not (any (\c -> txOutRefReceeivedFunds == C.txOutRef c ) (toAscList commands)))
      (toAscList allReceivedFunds)


data State = State
             { settings :: PlanSettings
             , fundRange :: Interval Ada
             , investorRef :: WhiteListedInvestorRef
             , volumes :: AddressVolumes
             , commands :: OSet Command} deriving (Show,Eq)

transition :: ReceivedFunds -> State -> State
transition ReceivedFunds {funds = Left _} State {..}
    = ignoreFunds -- Funds With Native Tokens
    -- Here : only ADAs
    where
        ignoreFunds  = State {  .. }
transition
    ReceivedFunds {funds = Right adas, ..}
    State { settings = settings@Settings {..} , volumes = volumes@AddressVolumes {..} ,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds 
    |  fundsBelowMinimumRequired       = ignoreFunds -- to low to be refund
    |  notBelongToRoundTimeRange       = appendCommand $ Refund {reason = TransactionOutofRoundTimeRange,refundAmount = adas,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds
    |  adressAlreadySaturated          = appendCommand $ Refund {reason = AddressSaturated,refundAmount = adas,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non already saturated address
    |  adressSaturatedWithIncomingFund = appendCommand $ SendOnExchangeAddressWithPartialRefund
                                                        { refundAmount = fundsOverSaturation
                                                        , adasToSendOnExchange = adas - fundsOverSaturation, ..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non saturated address with incoming fund
    |  otherwise                       = appendCommand $ SendOnExchangeAddress {adasToSendOnExchange = adas,..}
    where

        notBelongToRoundTimeRange = not $ I.member receivedAt timeRange
        fundsBelowMinimumRequired = before  adas fundRange
        adressAlreadySaturated = maximumAdaPerAddress < sent + accumulatedFundsToSent 
        adressSaturatedWithIncomingFund -- if Refund or Adas Sent to Exchange < minimumAdaPerFund we send on exchange 
            = maximumAdaPerAddress  < sent + accumulatedFundsToSent + adas 
            && minimumAdaPerFund    < fundsOverSaturation 
            && minimumAdaPerFund    < adas - fundsOverSaturation
        fundsOverSaturation = sent + accumulatedFundsToSent + adas - maximumAdaPerAddress
        accumulatedFundsToSent = sumAdaFunds $ toAscList commands
        appendCommand command = State { commands = commands |> command    , .. }
        ignoreFunds  = State {  .. }



sumAdaFunds :: [Command] -> Ada
sumAdaFunds xs = sum (getAdas <$> xs)





