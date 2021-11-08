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
    (plan) where
import           Prelude hiding (round,print)

import           Data.Set.Ordered

import           Plutus.V1.Ledger.Ada
import           Ledger.Ada as Ada
import           Plutus.V1.Ledger.Interval as I

import           Tokenomia.ICO.Funds.Reception.Types


plan :: RoundSettings -> AddressFundsState  ->  AddressFundsPlan
plan  round AddressFundsState {..}  
    = let State {commands} = foldr 
                                transition 
                                State {commands = empty,.. } 
                                transactions
    in Plan 
      { references = references
      , commands = commands}

data State = State 
             { round :: RoundSettings 
             , volumes :: AddressVolumes
             , commands :: OSet Command}

transition :: FundsTransaction -> State -> State
transition i@FundsTransaction {funds = Left _} State {..} 
    =                                   addCommand $ Reject i FundsWithNativeTokens
    -- Here : only ADAs
    where
        addCommand command = State { commands = command |< commands , .. }
transition
    i@FundsTransaction {funds = Right adas, ..}
    State { round = round@RoundSettings {..} , volumes = volumes@AddressVolumes {..} ,..}
    |  notBelongToRoundTimeRange       = addCommand $ Reject i TransactionOutofRoundTimeRange
    -- Here : only ADAs | transaction slot ∈ time range 
    |  fundsBelowMinimumRequired       = addCommand $ Reject i InsufficientFundsReceived
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds 
    |  adressAlreadySaturated          = addCommand $ Reject i AddressSaturated
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non already saturated address
    |  adressSaturatedWithIncomingFund = addCommand $ AcceptWithPartialRefund i fundsOverSaturation
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non saturated address with incoming fund
    |  notBelongToFundRange            = addCommand $ AcceptWithPartialRefund i (fundsOverMaximumRequired fundRange)
    -- Here : only ADAs | transaction slot ∈ time range | funds ∈ fundsRange | in a non saturated address with incoming fund
    |  otherwise                       = addCommand $ Accept i
    where

        notBelongToRoundTimeRange = not $ I.member transactionSlot timeRange
        notBelongToFundRange      = not $ I.member (Ada.lovelaceOf (fromIntegral adas)) fundRange
        fundsBelowMinimumRequired = before (Ada.lovelaceOf (fromIntegral adas)) fundRange
        adressAlreadySaturated = maximumAdaPerAdress < (sent + accumulatedFundsToSent)
        adressSaturatedWithIncomingFund = maximumAdaPerAdress < sent + accumulatedFundsToSent + sumAdaFunds [i]
        accumulatedFundsToSent = sumAdaFunds (transaction <$> toAscList commands)
        fundsOverSaturation = sent + accumulatedFundsToSent + sumAdaFunds [i] - maximumAdaPerAdress
        fundsOverMaximumRequired (Interval _ (UpperBound (Finite maximumAdaPerTx) _) ) = Ada.lovelaceOf (fromIntegral adas) - maximumAdaPerTx
        fundsOverMaximumRequired _ = Ada.lovelaceOf 0
        addCommand command = State { commands = command |< commands , .. }



sumAdaFunds :: [FundsTransaction] -> Ada
sumAdaFunds [] = Ada.lovelaceOf 0
sumAdaFunds (FundsTransaction {funds = Left _} : xs)  = sumAdaFunds xs
sumAdaFunds (FundsTransaction {funds = Right x} : xs) = x + sumAdaFunds xs

