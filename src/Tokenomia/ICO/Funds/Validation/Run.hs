{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.ICO.Funds.Validation.Run (
  dryRun,
  run,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Prelude hiding (print, round)

import Tokenomia.Common.Shell.Console (printLn)

import Data.Function ((&))
import Data.List.NonEmpty qualified as NEL
import Data.Maybe
import Data.Set.NonEmpty qualified as NES
import Streamly.Internal.Data.Fold qualified as SF
import Streamly.Prelude qualified as S
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.PageNumber
import Tokenomia.Common.Transacting
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Command qualified as CardanoCLI
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Convert as CardanoCLICommand
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan qualified as CardanoCLI
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Transact
import Tokenomia.ICO.Funds.Validation.ChildAddress.State
import Tokenomia.ICO.Funds.Validation.Investor.Plan as Plan
import Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings
import Tokenomia.ICO.Funds.Validation.Status
import Tokenomia.ICO.Funds.WhiteListing.Repository
import Tokenomia.ICO.Round.Settings
import Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress

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

  S.drain $
    streamCommandsToTransact round
      --  & S.take 1 -- TODO : to be removed
      & S.mapM (buildTx roundAddresses)
      & S.mapM
        ( \BuiltTx {estimatedFees} -> do
            printLn $ "Tx Fees : " <> show estimatedFees
            printLn "--------------------------------------"
        )

  printLn "------------------------------------------------"
  printLn "- Investor's Funds Validation Ended (dry run)  "
  printLn "------------------------------------------------"

run ::
  ( MonadIO m
  , S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m ()
run round@RoundSettings {addresses = roundAddresses} = do
  let nbTxSentInParrallel = 500
  printLn $ show round
  S.drain $
    streamCommandsToTransact round
      --  & S.take 1 -- TODO : to be removed
      & S.mapM (transactWithoutConfirmation roundAddresses)
      & S.chunksOf nbTxSentInParrallel SF.toList
      & S.mapM (return . NEL.fromList)
      & S.mapM (waitConfirmation . NEL.last)
  printLn "--------------------------------------"
  printLn "- Investor's Funds Validation Ended   "
  printLn "--------------------------------------"

streamCommandsToTransact ::
  ( MonadIO m
  , S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  S.SerialT m (CardanoCLI.Plan CardanoCLI.Command)
streamCommandsToTransact round@RoundSettings {addresses = roundAddresses, investorsWallet = wallet@Wallet {name = investorsWallet}} = do
  let nbFundsPerTx = 7
  S.fromList (PageNumber <$> [1 ..])
    & S.mapM (\pageNumber -> fetchActiveAddresses roundAddresses pageNumber wallet)
    & S.takeWhile isJust
    & S.map fromJust
    & S.mapM (fetchByAddresses investorsWallet)
    & S.mapM (fetchAllWhiteListedInvestorRef round)
    & S.mapM (fetchAllWhiteListedFunds round)
    & S.map (fmap (mkPlan $ mkPlanSettings round))
    & S.mapM (displayInvestorPlans round)
    & S.mapM (CardanoCLICommand.convertInvestorPlans round)
    & S.concatMap S.fromList
    & S.chunksOf nbFundsPerTx SF.toList
    & S.mapM (return . NES.fromList . NEL.fromList) -- TODO : could break ?...
    & S.map (CardanoCLI.mkPlan Nothing)
    & S.mapM
      ( \planWithoutFees@CardanoCLI.Plan {commands} -> do
          fees <- estimatedFees <$> buildTx roundAddresses planWithoutFees
          printLn $ "Tx Fees : " <> show fees
          printLn $ "> " <> (show . length) commands <> " commands will be sent : \n"
          let planWithFees@CardanoCLI.Plan {commands = commandsWithDeductedFees} = CardanoCLI.mkPlan (Just fees) commands
          mapM_ (printLn .show) commandsWithDeductedFees
          return planWithFees
      )
