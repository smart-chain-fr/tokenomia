{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.Run
    (dryRun,run) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import           Tokenomia.Common.Shell.Console (printLn)

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Round.Settings
import qualified Streamly.Prelude as S
import           Tokenomia.ICO.Funds.Exchange.ReceivedFunds
import           Tokenomia.ICO.Funds.Exchange.Tokens
import           Tokenomia.ICO.Funds.Exchange.Plan
import           Tokenomia.ICO.Funds.Exchange.Transact
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set.NonEmpty as NES
import           Tokenomia.Common.Transacting
import           Tokenomia.Common.Token

run
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
run round@RoundSettings {addresses = roundAddresses} = do

    printLn $ show round
    
    printLn "Exchange "
    fetchRawReceivedFundsByTx round 
        >>= authentifyTxsAsComingFromRoundWallet round
        >>= discardRejectedTxs
        >>= \funds -> do
               tokensMaybe <- fetchTokens round   
               fees <- planAndEstimate round tokensMaybe funds
               printLn $ "Plan estimated : tokens " <> show tokensMaybe 
               let planWithFees =  mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired (Just fees) tokensMaybe (NES.fromList funds) 
               transact roundAddresses planWithFees
        >> printLn "Exchange Done"

dryRun
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
dryRun round@RoundSettings {addresses = roundAddresses} = do

    printLn $ show round

    printLn "Exchange "
    fetchRawReceivedFundsByTx round 
        >>= authentifyTxsAsComingFromRoundWallet round
        >>= discardRejectedTxs
        >>= \funds -> do
               tokensMaybe <- fetchTokens round   
               fees <- planAndEstimate round tokensMaybe funds
               printLn $ "Plan estimated : tokens " <> show tokensMaybe 
               let planWithFees =  mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired (Just fees) tokensMaybe (NES.fromList funds) 
               buildTx roundAddresses planWithFees
        >> printLn "Exchange Done"


planAndEstimate 
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings 
    -> Maybe ExchangeToken
    -> NEL.NonEmpty AuthentifiedFunds
    -> m Fees
planAndEstimate round@RoundSettings{..} tokensMaybe funds = do
    let plan  = mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired Nothing tokensMaybe (NES.fromList funds)
    printLn $ show plan
    estimatedFees <$> buildTx addresses plan  