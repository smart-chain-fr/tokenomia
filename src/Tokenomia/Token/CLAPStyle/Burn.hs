{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Token.CLAPStyle.Burn (burn) where

import           Prelude
import           Shh

import           Control.Monad.Reader


import qualified Data.Text as T

import           Ledger.Value



import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral as Wallet


load SearchPath ["echo", "printf"]

burn :: (MonadIO m, MonadReader Environment m)  => m ()
burn = do
    liftIO $ echo "Select the burner's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just burnerWallet@Wallet {paymentAddress = burnerAddr,..} -> do 
                Wallet.getCollateral burnerWallet
                    >>= \case
                        Nothing -> liftIO $ printf "Please create a collateral\n"
                        Just utxoWithCollateral -> do
                            liftIO $ echo "> Select the utxo containing ADAs for fees (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                            Wallet.selectUTxO burnerWallet
                                >>= \case 
                                    Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                    Just utxoWithFees -> do
                                        liftIO $ echo "> Select the utxo containing the token to burn :" 
                                        Wallet.selectUTxOFilterBy containingOneToken burnerWallet 
                                            >>= \case  
                                                Nothing -> liftIO $ echo "Tokens not found in your wallet."
                                                Just utxoWithToken  -> do
                                                    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
                                                    getMonetaryPolicyPath tokenPolicyHash
                                                        >>= \case 
                                                        Nothing -> liftIO $ echo "You can only burn token minted via tokenomia (Monetary Policy existing in ~/.tokenomia-cli/transactions/ )"
                                                        Just monetaryScriptFilePath -> do
                                                            amountToBurn  <- liftIO $ echo "-n" "> Amount to burn : "  >>  read @Integer <$> getLine
                                                            run_tx paymentSigningKeyPath
                                                                [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken 
                                                                , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                                , "--tx-out" , burnerAddr <> " + 1344798 lovelace + " <> show (totalAmount - amountToBurn) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
                                                                , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                                                , "--change-address"  , burnerAddr
                                                                , "--mint" , "-" <> show amountToBurn <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
                                                                , "--mint-script-file" , monetaryScriptFilePath
                                                                , "--mint-redeemer-value",  "[]"]

