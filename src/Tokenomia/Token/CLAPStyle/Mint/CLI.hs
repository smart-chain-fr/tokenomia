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

module Tokenomia.Token.CLAPStyle.Mint.CLI
    ( mint ) where
    
import           Prelude
import           Shh

import           Control.Monad.Reader

import qualified Data.Text as T

import qualified Data.ByteString.UTF8 as BSU 


import           Ledger hiding (mint)
import qualified Ledger.Value as L

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Wallet.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import qualified Tokenomia.Wallet.CLI as Wallet
import Control.Monad.Catch ( MonadMask )

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat"]

mint :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()
mint = do
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just wallet -> mint' wallet 

mint' 
    :: (MonadMask m,MonadIO m, MonadReader Environment m)    
    => Wallet
    -> m ()
mint' wallet@Wallet {paymentAddress = minterAddr,..} = do

    tokenNameToMint  <- liftIO $ echo "-n" "> Token Name : "             >>  L.tokenName . BSU.fromString <$> getLine
    amountToMint     <- liftIO $ echo "-n" "> Total Supply to Mint : "   >>  read @Integer <$> getLine

    liftIO $ echo "> Select the collateral utxo :" 
    selectUTxO wallet
        >>= \case 
            Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
            Just utxoWithCollateral -> do 
                liftIO $ echo "> Select the utxo used for Minting the Token :" 
                selectUTxO wallet
                >>= \case  
                    Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                    Just utxoForMinting -> do 
                        let monetaryPolicy = mkMonetaryPolicyScript 
                                                Params { txOutRefToConsume = txOutRef utxoForMinting
                                                       , amount = amountToMint
                                                       , tokenName = tokenNameToMint }
                            policyhash = scriptCurrencySymbol monetaryPolicy

                        liftIO $ echo "-------------------------"
                        liftIO $ echo $ "Policy hash will be : " <> show policyhash
                        liftIO $ echo "-------------------------"

                        monetaryScriptFilePath <- register_minting_script_file monetaryPolicy
                        run_tx paymentSigningKeyPath
                                [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForMinting 
                                , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                , "--change-address"  , minterAddr
                                , "--mint" , show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                , "--mint-script-file" , monetaryScriptFilePath
                                , "--mint-redeemer-value",  "[]"]
                                
                        liftIO $ echo "------------------------------------------------------"
                        liftIO $ echo "Done"
                        liftIO $ echo "------------------------------------------------------"
