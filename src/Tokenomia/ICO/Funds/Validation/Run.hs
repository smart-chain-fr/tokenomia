{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Validation.Run
    (dryRun
    ,run) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import           Tokenomia.Common.Shell.Console (printLn)

import           Tokenomia.Common.Environment
import           Tokenomia.ICO.Funds.Validation.ChildAddress.State
import           Tokenomia.ICO.Funds.Validation.Investor.Plan as Plan
import           Tokenomia.Wallet.Type
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Round.Settings
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress
import            Tokenomia.ICO.Funds.Validation.CardanoCLI.Convert as CardanoCLICommand
import qualified  Tokenomia.ICO.Funds.Validation.CardanoCLI.Command as CardanoCLI
import qualified  Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan as CardanoCLI
import           Tokenomia.ICO.Funds.Validation.CardanoCLI.Transact
import qualified Data.List.NonEmpty as NEL
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF
import Tokenomia.Common.PageNumber
import Data.Function ( (&) )
import Data.Maybe
import qualified Data.Set.NonEmpty as NES
import Tokenomia.ICO.Funds.WhiteListing.Repository
import Tokenomia.ICO.Funds.Validation.Status
import           Tokenomia.Common.Transacting
import           Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings

dryRun
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
dryRun round@RoundSettings {addresses = roundAddresses} = do
    printLn $ show round

    S.drain
         $ streamCommandsToTransact round
         & S.map (CardanoCLI.mkPlan Nothing)
         & S.mapM (buildTx roundAddresses)
         & S.mapM (\BuiltTx{estimatedFees} -> do 
            printLn $ "Tx Fees : " <> show estimatedFees
            printLn "--------------------------------------")

    printLn "------------------------------------------------"
    printLn "- Investor's Funds Validation Ended (dry run)  "
    printLn "------------------------------------------------"

run
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => RoundSettings 
       -> m ()
run round@RoundSettings {addresses = roundAddresses} = do

    printLn $ show round

    S.drain
         $ streamCommandsToTransact round
         & S.take 1 -- TODO : to be removed
         & S.map (CardanoCLI.mkPlan Nothing)
         & S.mapM (\planWithoutFees@CardanoCLI.Plan{commands} -> do 
             fees <- estimatedFees <$> buildTx roundAddresses planWithoutFees
             return $ CardanoCLI.mkPlan (Just fees) commands )
         & S.mapM (transact roundAddresses)

    printLn "--------------------------------------"
    printLn "- Investor's Funds Validation Ended   "
    printLn "--------------------------------------"

streamCommandsToTransact
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings
    -> S.SerialT m (NES.NESet CardanoCLI.Command)
streamCommandsToTransact round@RoundSettings {addresses = roundAddresses,investorsWallet = wallet@Wallet{name = investorsWallet}} = do
    let nbFundsPerTx = 10
    S.fromList (PageNumber <$> [1..])
         & S.mapM (\pageNumber -> fetchActiveAddresses roundAddresses pageNumber wallet)
         & S.takeWhile isJust & S.map fromJust
         & S.mapM (fetchByAddresses investorsWallet)
         & S.mapM fetchAllWhiteListedInvestorRef
         & S.mapM fetchAllWhiteListedFunds
         & S.map  (fmap (mkPlan $ mkPlanSettings round))
         & S.mapM displayInvestorPlans
         & S.mapM (CardanoCLICommand.convertInvestorPlans round)
         & S.concatMap S.fromList
         & S.chunksOf nbFundsPerTx SF.toList
         & S.mapM (\commands -> do
            printLn $ "> " <> (show . length)  commands <> " commands will be sent : \n"
            mapM_ (printLn .show) commands 
            (return . NES.fromList . NEL.fromList) commands) -- TODO : could break ?...
