{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Exchange.Run (
  dryRun,
  run,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Prelude hiding (print, round)

import Tokenomia.Common.Shell.Console (printLn)

import Data.List.NonEmpty qualified as NEL
import Data.Set.NonEmpty qualified as NES
import Streamly.Prelude qualified as S
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.Token
import Tokenomia.Common.Transacting
import Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command as C
import Tokenomia.ICO.Funds.Exchange.CardanoCLI.Convert
import Tokenomia.ICO.Funds.Exchange.CardanoCLI.Transact
import Tokenomia.ICO.Funds.Exchange.Plan as P
import Tokenomia.ICO.Funds.Exchange.ReceivedFunds
import Tokenomia.ICO.Funds.Exchange.Tokens
import Tokenomia.ICO.Round.Settings

run ::
  ( MonadIO m
  , S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m ()
run round@RoundSettings {addresses = roundAddresses} = do
  printLn $ show round
  printLn "------------------------------------------------"
  printLn "- Funds Exchange (run)  "
  printLn "------------------------------------------------"

  allFunds <-
    fetchRawReceivedFundsByTx round
      >>= authentifyTxsAsComingFromRoundWallet round
      >>= discardRejectedTxs
  -- funds > csv
  roundSpecificPlanWithFees <- fetchNextPlan round allFunds
  _ <- transact roundAddresses roundSpecificPlanWithFees
  printLn "------------------------------------------------"
  printLn "- Funds Exchange Ended "
  printLn "------------------------------------------------"
  run round

dryRun ::
  ( MonadIO m
  , S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m ()
dryRun round@RoundSettings {addresses = roundAddresses} = do
  printLn $ show round

  printLn "------------------------------------------------"
  printLn "- Funds Exchange (dry run)  "
  printLn "------------------------------------------------"
  printLn "1"
  allFunds <-
    fetchRawReceivedFundsByTx round
      >>= \a ->
        do
          printLn "a"
          authentifyTxsAsComingFromRoundWallet round a
          >>= \b -> do
            printLn "b"
            discardRejectedTxs b
  printLn "2"
  roundSpecificPlanWithFees <- fetchNextPlan round allFunds
  _ <- buildTx roundAddresses roundSpecificPlanWithFees

  printLn "------------------------------------------------"
  printLn "- Funds Exchange Ended (dry run)  "
  printLn "------------------------------------------------"

fetchNextPlan ::
  ( MonadIO m
  , S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  NEL.NonEmpty AuthentifiedFunds ->
  m (Plan Command)
fetchNextPlan round@RoundSettings {} allFunds = do
  let nbFundsPerTx = 68 --2550
  let nextFunds = NEL.fromList . NEL.take nbFundsPerTx . NEL.sort $ allFunds
  tokensMaybe <- fetchTokens round
  fees <- planAndEstimate round tokensMaybe nextFunds
  printLn $ "Remaining Tokens > " <> show tokensMaybe
  printLn $ "Estimated Fees   > " <> show fees
  let roundAgnosticPlanWithFees = mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired (Just fees) tokensMaybe (NES.fromList nextFunds)
  roundSpecificPlanWithFees@Plan {commands = cs} <- convertToRoundSpecificPlan round roundAgnosticPlanWithFees
  printLn $ show roundSpecificPlanWithFees
  mapM_ (printLn . show) cs
  return roundSpecificPlanWithFees

planAndEstimate ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  Maybe ExchangeToken ->
  NEL.NonEmpty AuthentifiedFunds ->
  m Fees
planAndEstimate round@RoundSettings {..} tokensMaybe funds = do
  let roundAgnosticPlanWithoutFees = mkPlan (mkPlanSettings round) getMinimumUTxOAdaRequired Nothing tokensMaybe (NES.fromList funds)
  roundSpecificPlanWithoutFees <- convertToRoundSpecificPlan round roundAgnosticPlanWithoutFees
  estimatedFees <$> buildTx addresses roundSpecificPlanWithoutFees
