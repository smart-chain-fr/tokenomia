{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.DryRun
    (dryRun) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import Data.List (intersperse)
import           Plutus.V1.Ledger.Ada

import           Tokenomia.Common.Shell.Console (printLn)

import Data.Foldable
import           Plutus.V1.Ledger.Interval
import           Tokenomia.Common.Environment
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State
import           Tokenomia.ICO.Funds.Reception.Plan
import           Data.Set.Ordered hiding (null)
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.LocalRepository as Wallet
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.ICO.RoundSettings
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAdddress
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import           Tokenomia.Common.Address ( Address(..) )


dryRun
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => m ()
dryRun = do
    wallet@Wallet {name} <- Wallet.fetchAll >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "\nSelect a wallet : "
            askToChooseAmongGivenWallets wallets

    let round = RoundSettings
                { maximumAdaPerAdress = adaOf 50
                , fundRange = interval (adaOf 2) (adaOf 1000)
                , timeRange = interval 40354960 43002258
                , wallet = wallet
                , exchangeAddressRef = ChildAddressRef name 0
                , collateralAddressRef = ChildAddressRef name 1}

    addresses  <- fetchActiveAddresses wallet
    indexedAddresses <- fetchIndexedAddresses name addresses 
    let fakeWhitelistedInvestorRefs = WhiteListedInvestorRef (Address "addr_test_fake_exchangePaybackAddress") <$> indexedAddresses
    whiteListedInvestorStates <- fetchAllWhiteListedFunds fakeWhitelistedInvestorRefs
    printLn "|| Round Context  ||"
    printLn $ show round
    printLn "--------------------------------------"
    let statesAndplans = (\a -> (a,plan round a)) <$> whiteListedInvestorStates
    sequence_ $ uncurry displayStateAndPlan  <$> statesAndplans
    printLn "--------------------------------------"

displayStateAndPlan :: ( MonadIO m) => WhiteListedInvestorState ->  AddressFundsPlan  -> m ()
displayStateAndPlan WhiteListedInvestorState {allReceivedFunds,volumes} Plan {investorRef,commands} | not (null allReceivedFunds)
    = do
    printLn $ "|| Investor ||\n" <> show investorRef
    printLn   "\n|| Address Status ||\n" 
    printLn   "> Volumes :" 
    printLn $ show volumes
    printLn $ "\n> Funds Received : " <> (show . length) allReceivedFunds
    printLn $ fold (intersperse "\n" (show <$> toAscList allReceivedFunds))
    printLn   "\n> Plan :\n"
    printLn $ fold (intersperse "\n" (show <$> toAscList commands))
    printLn   "---------"
displayStateAndPlan _ Plan {..}
    = do
    printLn $ show investorRef
    printLn "> no Funds Received"
    printLn "---------"
 


