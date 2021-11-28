{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.Plan
    ( plan
    , AddressFundsPlan (..)) where
import           Prelude hiding (round,print)

import           Data.Set.Ordered

import           Plutus.V1.Ledger.Ada
import           Ledger.Ada as Ada
import           Plutus.V1.Ledger.Interval as I

import           Tokenomia.ICO.Funds.Reception.Command
import           Tokenomia.ICO.RoundSettings
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types

data AddressFundsPlan = Plan {investorRef :: WhiteListedInvestorRef , commands :: OSet Command} 

plan :: RoundSettings -> WhiteListedInvestorState  ->  AddressFundsPlan
plan  round WhiteListedInvestorState {..}  
    = let State {commands} = foldr 
                                transition 
                                State {commands = empty,.. } 
                                allReceivedFunds
    in Plan 
      { investorRef = investorRef
      , commands = commands}

data State = State 
             { round :: RoundSettings
             , investorRef :: WhiteListedInvestorRef 
             , volumes :: AddressVolumes
             , commands :: OSet Command}

transition :: ReceivedFunds -> State -> State
transition ReceivedFunds {funds = Left nativeTokens,..} State {..} 
    =                                   addCommand $ RejectFundsWithNativeTokens {..}
    -- Here : only ADAs
    where
        addCommand command = State { commands = command |< commands , .. }
transition
    ReceivedFunds {funds = Right adas, ..}
    State { round = round@RoundSettings {..} , volumes = volumes@AddressVolumes {..} ,..}
    |  notBelongToRoundTimeRange       = addCommand $ Reject {reason = TransactionOutofRoundTimeRange,..}  
    -- Here : only ADAs | transaction slot ∈ time range 
    |  fundsBelowMinimumRequired       = addCommand $ Reject {reason = InsufficientFundsReceived,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds 
    |  adressAlreadySaturated          = addCommand $ Reject {reason = AddressSaturated,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non already saturated address
    |  adressSaturatedWithIncomingFund = addCommand $ AcceptWithPartialRefund {refundAmount = fundsOverSaturation,..} 
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non saturated address with incoming fund
    |  notBelongToFundRange            = addCommand $ AcceptWithPartialRefund { refundAmount = fundsOverMaximumRequired fundRange,..}
    -- Here : only ADAs | transaction slot ∈ time range | funds ∈ fundsRange | in a non saturated address with incoming fund
    |  otherwise                       = addCommand $ Accept {..}
    where

        notBelongToRoundTimeRange = not $ I.member receivedAt timeRange
        notBelongToFundRange      = not $ I.member (Ada.lovelaceOf (fromIntegral adas)) fundRange
        fundsBelowMinimumRequired = before (Ada.lovelaceOf (fromIntegral adas)) fundRange
        adressAlreadySaturated = maximumAdaPerAdress < (sent + accumulatedFundsToSent)
        adressSaturatedWithIncomingFund = maximumAdaPerAdress < sent + accumulatedFundsToSent + adas
        accumulatedFundsToSent = sumAdaFunds $ toAscList commands
        fundsOverSaturation = sent + accumulatedFundsToSent + adas - maximumAdaPerAdress
        fundsOverMaximumRequired (Interval _ (UpperBound (Finite maximumAdaPerTx) _) ) = Ada.lovelaceOf (fromIntegral adas) - maximumAdaPerTx
        fundsOverMaximumRequired _ = Ada.lovelaceOf 0
        addCommand command = State { commands = command |< commands , .. }



sumAdaFunds :: [Command] -> Ada
sumAdaFunds [] = Ada.lovelaceOf 0
sumAdaFunds (RejectFundsWithNativeTokens {} : xs)  = sumAdaFunds xs
sumAdaFunds (Reject {adas} : xs) = adas + sumAdaFunds xs
sumAdaFunds (AcceptWithPartialRefund {adas} : xs) = adas + sumAdaFunds xs
sumAdaFunds (Accept {adas} : xs) = adas + sumAdaFunds xs


