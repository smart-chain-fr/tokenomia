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

module Tokenomia.Token.CLAPStyle.Mint
    ( mint ) where
    
import           Prelude
import           Shh

import           Control.Monad.Reader

import qualified Data.Text as T

import qualified Data.ByteString.UTF8 as BSU 


import           Ledger hiding (mint)
import qualified Ledger.Value as L

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Wallet.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral as Wallet
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts

load SearchPath ["echo", "printf"]

mint :: (MonadIO m, MonadReader Environment m)  => m ()
mint = do
    liftIO $ echo "Select the minter's wallet" 
    Wallet.askAmongAllWallets
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just wallet@Wallet {paymentAddress = minterAddr,..} -> do 
                Wallet.getCollateral wallet
                    >>= \case
                        Nothing -> liftIO $ printf "Please create a collateral\n"
                        Just utxoWithCollateral -> do
                            liftIO $ echo "> Select the utxo containing ADAs for fees (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                            Wallet.askUTxO wallet
                                >>= \case 
                                    Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                    Just utxoWithFees -> do
                                        tokenNameToMint  <- liftIO $ echo "-n" "> Token Name : "             >>  L.tokenName . BSU.fromString <$> getLine
                                        amountToMint     <- liftIO $ echo "-n" "> Total Supply to Mint : "   >>  read @Integer <$> getLine
                                        liftIO $ echo "> Select the utxo used for Minting the Token (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                                        askUTxO wallet
                                            >>= \case  
                                                Nothing -> liftIO $ echo "Please, add a tokens to your wallet"
                                                Just utxoForMinting -> do 
                                                    let monetaryPolicy = mkMonetaryPolicyScript 
                                                                            Params { txOutRefToConsume = txOutRef utxoForMinting
                                                                                , amount = amountToMint
                                                                                , tokenName = tokenNameToMint }
                                                        policyhash = scriptCurrencySymbol monetaryPolicy

                                                    liftIO $ echo "-------------------------"
                                                    liftIO $ echo $ "Policy hash will be : " <> show policyhash
                                                    liftIO $ echo "-------------------------"

                                                    monetaryScriptFilePath <- registerMintingScriptFile monetaryPolicy
                                                    submit paymentSigningKeyPath utxoWithFees
                                                            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForMinting 
                                                            , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                            , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                                            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                                            , "--change-address"  , minterAddr
                                                            , "--mint" , show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                                            , "--mint-script-file" , monetaryScriptFilePath
                                                            , "--mint-redeemer-value",  "[]"]
                                            
