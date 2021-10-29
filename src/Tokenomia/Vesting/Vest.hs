{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Vesting.Vest
    ( vestFunds) where

import           Prelude hiding ((+),(-))
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIX
import           Control.Monad.Reader
import           Control.Monad.Except
import           Shh

import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))
import           Ledger.Value 
import           Plutus.V1.Ledger.Ada


import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO

import           Tokenomia.Vesting.Contract
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Vesting.Repository
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Error

load SearchPath ["echo"]

vestFunds
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => m ()
vestFunds = do
    ownerWallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            liftIO $ echo "Select the token owner wallet : "
            askToChooseAmongGivenWallets wallets 
    utxosWithOneToken <- fetchUTxOFilterBy containingOneToken ownerWallet >>= whenNothingThrow NoUTxOWithOnlyOneToken
    liftIO $ echo "- Select the utxo and associated tokens to vest  :" 
    utxoWithToken <- askToChooseAmongGivenUTxOs utxosWithOneToken
    liftIO $ echo "- First Tranche : "
    nbSecondsTranche1  <- liftIO $ echo "-n" "> How many seconds will you vest the tokens ? : "  >>  read @Integer <$> getLine
    nbTokenTranche1  <- liftIO $ echo "-n" "> How many tokens will you vest ? : "  >>  read @Integer <$> getLine
   
    liftIO $ echo "- Second Tranche : "
    nbSecondsTranche2  <- liftIO $ echo "-n" "> How many seconds will you vest the tokens ? : "  >>  read @Integer <$> getLine
    nbTokenTranche2  <- liftIO $ echo "-n" "> How many tokens will you vest ? : "  >>  read @Integer <$> getLine

    liftIO $ echo "- Select the investor's wallet"
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
    utxoWithToken 
    (nbSecondsTranche1,nbTokenTranche1)
    (nbSecondsTranche2,nbTokenTranche2) = do

    collateral <- fetchCollateral ownerWallet >>= whenNothingThrow WalletWithoutCollateral  
    utxoForFees <- selectBiggestStrictlyADAsNotCollateral ownerWallet >>= whenNothingThrow NoADAInWallet

    let (cu,tn,_) = getTokenFrom utxoWithToken
    now <- liftIO POSIX.getPOSIXTime
    let params = VestingParams
                    {   vestingTranche1 = VestingTranche
                                { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche1)
                                , vestingTrancheAmount = singleton cu tn nbTokenTranche1 + lovelaceValueOf 1689618}
                    ,   vestingTranche2 = VestingTranche
                                { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche2)
                                , vestingTrancheAmount = singleton cu tn nbTokenTranche2 + lovelaceValueOf 1689618}
                    , vestingOwner = publicKeyHash investorWallet }
        
    let vscript = vestingScript params
        valueToBeVested    = (vestingTrancheAmount . vestingTranche1) params + (vestingTrancheAmount . vestingTranche2) params
        changeBackToOwner  = getTokensFrom (value utxoWithToken - valueToBeVested) + lovelaceValueOf 1689618
    scriptLocation <- registerValidatorScriptFile vscript

    datumVoidHash <- getDataHash ()
    submit (paymentSigningKeyPath ownerWallet) utxoForFees
            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
            , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees
            , "--tx-out" , onChain scriptLocation <> " " <> (T.unpack . toCLI .vestingTrancheAmount . vestingTranche1) params
            , "--tx-out-datum-hash"  , datumVoidHash 
            , "--tx-out" , onChain scriptLocation <> " " <> (T.unpack . toCLI .vestingTrancheAmount . vestingTranche2) params 
            , "--tx-out-datum-hash"  , datumVoidHash 
            , "--tx-out" , paymentAddress ownerWallet <> " " <> (T.unpack . toCLI) changeBackToOwner 
            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
            , "--change-address"  , paymentAddress ownerWallet
            ]

    liftIO $ echo   "-----------------------------------------"
    liftIO $ echo   "- Investor can retrieve the funds from :"
    liftIO $ echo $ "\t - Tranche 1 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche1) params
    liftIO $ echo $ "\t - Tranche 2 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche2) params
    liftIO $ echo $ "- Funds location (script address) : " <> onChain scriptLocation
    liftIO $ echo   "-----------------------------------------"
    
    register params
