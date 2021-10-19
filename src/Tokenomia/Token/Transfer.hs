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


module Tokenomia.Token.Transfer 
    ( transfer) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral as Wallet
import qualified Data.Text as T
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import Ledger.Value


{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","printf"]

transfer :: (MonadIO m, MonadReader Environment m)  => m ()
transfer = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just senderWallet@Wallet {paymentAddress = senderAddr,..} -> do 
                Wallet.getCollateral senderWallet
                    >>= \case
                        Nothing -> liftIO $ printf "Please create a collateral\n"
                        Just utxoWithCollateral -> do
                            receiverAddr    <- liftIO $ echo "-n" "> Receiver address : "  >>  getLine
                            liftIO $ echo "> Select the utxo containing ADAs for fees (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                            Wallet.selectUTxO senderWallet
                                >>= \case 
                                Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                Just utxoWithFees -> do 
                                    liftIO $ echo "> Select the utxo containing the token to transfer (please don't use the utxo containing 2 ADA as it is used for collateral) :" 
                                    Wallet.selectUTxOFilterBy containingOneToken senderWallet 
                                        >>= \case  
                                            Nothing -> liftIO $ echo "Tokens not found in your wallet."
                                            Just utxoWithToken  -> do
                                                let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
                                                amount <- liftIO $ echo "-n" "> Amount of Token : "   >>  read @Integer <$> getLine
                                                run_tx paymentSigningKeyPath 
                                                        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
                                                        , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                        , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                                                        , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalAmount - amount) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                                                        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral 
                                                        , "--change-address"  , senderAddr]



