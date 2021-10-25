{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Ada.Transfer 
    ( transfer ) where

import qualified Data.Text as T
import           Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral as Wallet
import           Tokenomia.Adapter.Cardano.CLI.Wallet

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo", "printf"]

transfer :: (MonadIO m, MonadReader Environment m)  => m ()
transfer = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.askAmongAllWallets
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just senderWallet@Wallet {paymentAddress = senderAddr,..} -> do 
                Wallet.getCollateral senderWallet
                    >>= \case
                        Nothing -> liftIO $ printf "Please create a collateral\n"
                        Just utxoWithCollateral -> do 
                            receiverAddr    <- liftIO $ echo "-n" "> Receiver address : "  >>  getLine
                            liftIO $ echo "> Select the utxo containing ADAs for fees (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                            Wallet.askUTxO senderWallet
                                >>= \case 
                                    Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                    Just utxoWithFees -> do 
                                        liftIO $ echo "> Select the utxo containing Ada to transfer (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                                        Wallet.askUTxOFilterBy containingStrictlyADAs senderWallet 
                                            >>= \case  
                                                Nothing -> liftIO $ echo "UTxO containing ONLY Ada not found in your wallet."
                                                Just utxoWithAda  -> do
                                                    amount          <- liftIO $ echo "-n" "> Amount of Ada (in lovelaces) : "   >>  read @Integer <$> getLine
                                                    submit paymentSigningKeyPath utxoWithFees
                                                            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithAda
                                                            , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                            , "--tx-out" , receiverAddr <> " " <> show amount <> " lovelace"
                                                            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral 
                                                            , "--change-address"  , senderAddr]


