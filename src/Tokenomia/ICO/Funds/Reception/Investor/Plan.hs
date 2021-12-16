{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.Investor.Plan
    ( mkPlan
    , mkPlan'
    , State (..)
    , InvestorPlan (..)) where
import           Prelude hiding (round,print)

import           Data.Set.Ordered

import           Plutus.V1.Ledger.Ada
import           Ledger.Ada as Ada
import           Plutus.V1.Ledger.Interval as I

import           Tokenomia.ICO.Funds.Reception.Investor.Command
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import           Tokenomia.ICO.Funds.Reception.Investor.Plan.Settings

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
                                allReceivedFunds
    in (s,InvestorPlan 
      { investorRef = investorRef
      , commands = commands})

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
    |  notBelongToRoundTimeRange       = addCommand $ Refund {reason = TransactionOutofRoundTimeRange,refundAmount = adas,..}  
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds
    |  adressAlreadySaturated          = addCommand $ Refund {reason = AddressSaturated,refundAmount = adas,..}
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non already saturated address
    |  adressSaturatedWithIncomingFund = addCommand $ SendOnExchangeAddressWithPartialRefund 
                                                        { refundAmount = fundsOverSaturation
                                                        , adasToSendOnExchange = adas - fundsOverSaturation, ..} 
    -- Here : only ADAs | transaction slot ∈ time range | minimum < funds | in a non saturated address with incoming fund
    |  notBelongToFundRange            = addCommand $ SendOnExchangeAddressWithPartialRefund 
                                                        { refundAmount = fundsOverMaximumRequired fundRange
                                                        , adasToSendOnExchange = adas - fundsOverSaturation, ..}
    -- Here : only ADAs | transaction slot ∈ time range | funds ∈ fundsRange | in a non saturated address with incoming fund
    |  otherwise                       = addCommand $ SendOnExchangeAddress {adasToSendOnExchange = adas,..}
    where
        
        notBelongToRoundTimeRange = not $ I.member receivedAt timeRange
        notBelongToFundRange      = not $ I.member (Ada.lovelaceOf (fromIntegral adas)) fundRange
        fundsBelowMinimumRequired = before  adas fundRange
        adressAlreadySaturated = maximumAdaPerAddress < (sent + accumulatedFundsToSent)
        adressSaturatedWithIncomingFund = maximumAdaPerAddress < sent + accumulatedFundsToSent + adas
        accumulatedFundsToSent = sumAdaFunds $ toAscList commands
        fundsOverSaturation = sent + accumulatedFundsToSent + adas - maximumAdaPerAddress
        fundsOverMaximumRequired (Interval _ (UpperBound (Finite maximumAdaPerTx) _) ) = Ada.lovelaceOf (fromIntegral adas) - maximumAdaPerTx
        fundsOverMaximumRequired _ = Ada.lovelaceOf 0
        addCommand command = State { commands = command |< commands , .. }
        ignoreFunds  = State {  .. }



sumAdaFunds :: [Command] -> Ada
sumAdaFunds xs = sum (getAdas <$> xs)





