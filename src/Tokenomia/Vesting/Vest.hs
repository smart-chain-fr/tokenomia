{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Vesting.Vest
    ( vestFunds) where

import           Prelude hiding ((+),(-), print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))
import qualified Prelude as P

import           Data.List.NonEmpty
import qualified Data.Time.Clock.POSIX as POSIX
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Ledger.Value 
import           Plutus.V1.Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.UTxO

import           Tokenomia.Vesting.Contract
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Transaction 

import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Vesting.Repository
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console
import           Tokenomia.Common.Shell.InteractiveMenu


vestFunds
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => m ()
vestFunds = do
    ownerWallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the token owner wallet :"
            askToChooseAmongGivenWallets wallets 
    utxosWithOneToken <- fetchUTxOFilterBy containingOneToken ownerWallet >>= whenNothingThrow NoUTxOWithOnlyOneToken
    printLn "- Select the utxo and associated tokens to vest  :" 
    utxoWithToken <- askToChooseAmongGivenUTxOs utxosWithOneToken

    printLn "- First Tranche :"
    nbSecondsTranche1  <- ask @Integer "> How many seconds will you vest the tokens ? : "
    nbTokenTranche1    <- ask @Integer "> How many tokens will you vest ? : "
   
    printLn "- Second Tranche :"
    nbSecondsTranche2  <- ask @Integer "> How many seconds will you vest the tokens ? : "
    nbTokenTranche2    <- ask @Integer "> How many tokens will you vest ? : "

    printLn "- Select the investor's wallet :"

    investorWallet <- askAmongAllWallets >>= whenNothingThrow NoWalletRegistered
    vestFunds' 
        ownerWallet
        investorWallet 
        utxoWithToken 
        (nbSecondsTranche1,nbTokenTranche1)
        (nbSecondsTranche2,nbTokenTranche2)

vestFunds'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => Wallet
    -> Wallet 
    -> UTxO 
    -> (Integer,Integer)
    -> (Integer,Integer)
    -> m ()
vestFunds'
    ownerWallet 
    investorWallet 
    utxoWithTokens
    (nbSecondsTranche1,nbTokenTranche1)
    (nbSecondsTranche2,nbTokenTranche2) = do

    let (cu,tn,totalAmountToken) = getTokenFrom utxoWithTokens
        valueTotalToken = singleton cu tn totalAmountToken
        valueTokenTranche1 =  singleton cu tn nbTokenTranche1
        valueTokenTranche2 = singleton cu tn nbTokenTranche2 
        valueToBeVested = valueTokenTranche1 + valueTokenTranche2
        changeBackToOwner  =  valueTotalToken - valueToBeVested

    now <- liftIO POSIX.getPOSIXTime
    let params = VestingParams
                    {   vestingTranche1 = VestingTranche
                                { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche1)
                                , vestingTrancheAmount = valueTokenTranche1 + lovelaceValueOf 1689618}
                    ,   vestingTranche2 = VestingTranche
                                { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche2)
                                , vestingTrancheAmount = valueTokenTranche2 + lovelaceValueOf 1689618}
                    , vestingOwner = publicKeyHash investorWallet }
        
    let vscript = vestingScript params
    scriptLocation <- registerValidatorScriptFile vscript

    datumVoidHash <- getDataHash ()

    submit'
      TxBuild
        { wallet = ownerWallet
        , txIns =  FromWallet (txOutRef  utxoWithTokens) :| []
        , txOuts = ToWallet (paymentAddress ownerWallet) (changeBackToOwner + lovelaceValueOf 1344798) 
                :| [ ToScript 
                     { address = onChain scriptLocation
                     , value =  (vestingTrancheAmount . vestingTranche1) params
                     , datumHash = datumVoidHash}
                   , ToScript 
                     { address = onChain scriptLocation
                     , value =  (vestingTrancheAmount . vestingTranche2) params
                     , datumHash = datumVoidHash}] 
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing 
        , ..}

    printLn   "-----------------------------------------"
    printLn   "- Investor can retrieve the funds from :"
    printLn $ "\t - Tranche 1 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche1) params
    printLn $ "\t - Tranche 2 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche2) params
    printLn $ "- Funds location : " <> show (onChain scriptLocation)
    printLn   "-----------------------------------------"
    
    register params
