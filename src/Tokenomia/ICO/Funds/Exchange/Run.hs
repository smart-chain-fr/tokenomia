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
import           Tokenomia.ICO.Funds.Exchange.CardanoCLI.Transact
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set.NonEmpty as NES
import           Tokenomia.Common.Transacting
import           Tokenomia.Common.Token
import           Tokenomia.ICO.Funds.Exchange.CardanoCLI.Convert
import           Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command
import qualified Streamly.Internal.Data.Fold as SF
import Data.Function ( (&) )

run
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
run round@RoundSettings {addresses = roundAddresses} = do

    printLn $ show round
    printLn "------------------------------------------------"
    printLn "- Funds Exchange (run)  "
    printLn "------------------------------------------------"   
    
    allFunds <- fetchRawReceivedFundsByTx round 
        >>= authentifyTxsAsComingFromRoundWallet round
        >>= discardRejectedTxs
    -- funds > csv 
    roundSpecificPlanWithFees <- fetchNextPlan round allFunds
    _ <- transact roundAddresses roundSpecificPlanWithFees   
    printLn "------------------------------------------------"
    printLn "- Funds Exchange Ended "
    printLn "------------------------------------------------" 
    


dryRun
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
dryRun round@RoundSettings {addresses = roundAddresses} = do

    printLn $ show round

    printLn "------------------------------------------------"
    printLn "- Funds Exchange (dry run)  "
    printLn "------------------------------------------------"   
    
    allFunds <- fetchRawReceivedFundsByTx round 
        >>= authentifyTxsAsComingFromRoundWallet round
        >>= discardRejectedTxs
     
    roundSpecificPlanWithFees <- fetchNextPlan round allFunds
    _ <- buildTx roundAddresses roundSpecificPlanWithFees   
    
    printLn "------------------------------------------------"
    printLn "- Funds Exchange Ended (dry run)  "
    printLn "------------------------------------------------"     
 

fetchNextPlan
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings
    -> NEL.NonEmpty AuthentifiedFunds
    -> m (Plan Command)
fetchNextPlan round@RoundSettings {} allFunds = do
    let nbFundsPerTx = 10
    let nextFunds = NEL.fromList . NEL.take nbFundsPerTx . NEL.sort $ allFunds 
    tokensMaybe <- fetchTokens round   
    fees <- planAndEstimate round tokensMaybe nextFunds
    printLn $ "Remaining Tokens > " <> show tokensMaybe 
    printLn $ "Estimated Fees   > " <> show fees
    let roundAgnosticPlanWithFees =  mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired (Just fees) tokensMaybe (NES.fromList funds) 
    roundSpecificPlanWithFees <- convertToRoundSpecificPlan round roundAgnosticPlanWithFees
    printLn $ show roundSpecificPlanWithFees
    return roundSpecificPlanWithFees   
    

planAndEstimate 
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings 
    -> Maybe ExchangeToken
    -> NEL.NonEmpty AuthentifiedFunds
    -> m Fees
planAndEstimate round@RoundSettings{..} tokensMaybe funds = do
    let roundAgnosticPlanWithoutFees  = mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired Nothing tokensMaybe (NES.fromList funds)
    roundSpecificPlanWithoutFees <- convertToRoundSpecificPlan round roundAgnosticPlanWithoutFees
    estimatedFees <$> buildTx addresses roundSpecificPlanWithoutFees  