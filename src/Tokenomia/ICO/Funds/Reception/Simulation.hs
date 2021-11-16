{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.Simulation
    (simulatePlan) where

import Prelude hiding (round,print)
import           Control.Monad.Reader



import Data.List (intersperse)
import           Plutus.V1.Ledger.Ada

import qualified Blockfrost.Client as B

import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.Common.Shell.Console (printLn)

import Data.Foldable
import           Plutus.V1.Ledger.Interval

import           Tokenomia.ICO.Funds.Reception.Types
import           Tokenomia.ICO.Funds.Reception.State
import           Tokenomia.ICO.Funds.Reception.Plan
import           Tokenomia.Common.Shell.InteractiveMenu  (askString)
import           Data.Set.Ordered hiding (null)

simulatePlan
    :: ( MonadIO m)
       => m ()
simulatePlan = do
    let round = RoundSettings
                { maximumAdaPerAdress = adaOf 50
                , fundRange = interval (adaOf 2) (adaOf 1000)
                , timeRange = interval 40354960 43002258 }
    address  <- Address <$> askString   "- Plan Simulation on address  : "

    prj <- liftIO B.projectFromEnv
    res <- liftIO $ B.runBlockfrost prj $ fetchStateFromStakeAddress address
    printLn "Round Context : "
    printLn $ show round
    printLn "--------------------------------------"
    case res of
        Right addressFundsStates-> do
            let statesAndplans = (\a -> (a,plan round a)) <$> addressFundsStates
            sequence_ $ uncurry displayStateAndPlan  <$> statesAndplans
        Left err ->
            printLn $ show err
    printLn "--------------------------------------"

displayStateAndPlan :: ( MonadIO m) => AddressFundsState ->  AddressFundsPlan  -> m ()
displayStateAndPlan AddressFundsState {transactions,volumes} Plan {references,commands} | not (null transactions)
    = do
    printLn $ show references
    printLn $ "- Volumes :" <> show volumes
    printLn $ "- Transactions not consumed : " <> (show . length) transactions
    printLn $ fold (intersperse "\n" (show <$> toAscList transactions))
    printLn "- Plan :"
    printLn $ fold (intersperse "\n" (show <$> toAscList commands))
    printLn "---------"
displayStateAndPlan _ Plan {..} 
    = do
    printLn $ show references
    printLn "> no transactions to consume"
    printLn "---------"
 