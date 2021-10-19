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

import           Shh

import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))
import           Ledger.Value 
import           Plutus.V1.Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import qualified Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Vesting.Contract
import           Tokenomia.Adapter.Cardano.CLI.Environment

load SearchPath ["echo"]

vestFunds :: (MonadIO m, MonadReader Environment m)  => m ()
vestFunds = do
    liftIO $ echo "Select the token's owner wallet"
    Wallet.select
        >>= \case
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just ownerWallet -> do
                liftIO $ echo "> Select the collateral utxo :"
                Wallet.selectUTxO ownerWallet
                    >>= \case
                        Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                        Just utxoWithCollateral -> do
                            liftIO $ echo "> Select the utxo containing ADAs for fees  :"
                            Wallet.selectUTxO ownerWallet
                            >>= \case
                                Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                Just utxoWithFees -> do
                                    liftIO $ echo "Select the investor's wallet"
                                    Wallet.select
                                        >>= \case
                                            Nothing -> liftIO $ print "No Wallet Registered !"
                                            Just investorWallet@Wallet{} -> do
                                                liftIO $ echo "> Select the utxo containing the token to vest  :"
                                                Wallet.selectUTxOFilterBy containingOneToken ownerWallet
                                                    >>= \case
                                                            Nothing -> liftIO $ echo "Tokens not found in your wallet."
                                                            Just utxoWithToken  -> do

                                                                liftIO $ echo "First Tranche : "
                                                                nbSecondsTranche1  <- liftIO $ echo "-n" "> How many seconds will you vest the tokens ? : "  >>  read @Integer <$> getLine
                                                                nbTokenTranche1  <- liftIO $ echo "-n" "> How many tokens will you vest ? : "  >>  read @Integer <$> getLine

                                                                liftIO $ echo "Second Tranche : "
                                                                nbSecondsTranche2  <- liftIO $ echo "-n" "> How many seconds will you vest the tokens ? : "  >>  read @Integer <$> getLine
                                                                nbTokenTranche2  <- liftIO $ echo "-n" "> How many tokens will you vest ? : "  >>  read @Integer <$> getLine

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
                                                                    
                                                                liftIO $ echo "Investor can retrieve the funds from :"
                                                                liftIO $ echo $ "\t - Tranche 1 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche1) params
                                                                liftIO $ echo $ "\t - Tranche 2 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche2) params

                                                                let vscript = vestingScript params
                                                                    valueToBeVested    = (vestingTrancheAmount . vestingTranche1) params + (vestingTrancheAmount . vestingTranche2) params
                                                                    changeBackToOwner  = getTokensFrom (value utxoWithToken - valueToBeVested) + lovelaceValueOf 1689618
                                                                scriptLocation <- registerValidatorScriptFile vscript

                                                                liftIO $ echo $ "Script adress will be  :" <> onChain scriptLocation
                                                                datumVoidHash <- getDataHash ()
                                                                run_tx (paymentSigningKeyPath ownerWallet)
                                                                        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
                                                                        , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees
                                                                        , "--tx-out" , onChain scriptLocation <> " " <> (T.unpack . toCLI .vestingTrancheAmount . vestingTranche1) params
                                                                        , "--tx-out-datum-hash"  , datumVoidHash 
                                                                        , "--tx-out" , onChain scriptLocation <> " " <> (T.unpack . toCLI .vestingTrancheAmount . vestingTranche2) params 
                                                                        , "--tx-out-datum-hash"  , datumVoidHash 
                                                                        , "--tx-out" , paymentAddress ownerWallet <> " " <> (T.unpack . toCLI) changeBackToOwner 
                                                                        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                                                        , "--change-address"  , paymentAddress ownerWallet
                                                                        ]
                                                                registerVestingIndex params

