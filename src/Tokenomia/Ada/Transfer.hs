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
    ( transfer) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
import Control.Monad.Catch ( MonadMask ) 
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Data.Text as T
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import Ledger.Value
import Plutus.V1.Ledger.Ada

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo"]

transfer :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()
transfer = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just senderWallet@Wallet {paymentAddress = senderAddr,..} -> do 
                receiverAddr    <- liftIO $ echo "-n" "> Receiver address : "  >>  getLine
                
                liftIO $ echo "> Select the collateral utxo :" 
                Wallet.selectUTxO senderWallet
                    >>= \case 
                        Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                        Just utxoWithCollateral -> do 
                            liftIO $ echo "> Select the utxo containing ADAs for fees  :" 
                            Wallet.selectUTxO senderWallet
                            >>= \case 
                                Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                Just utxoWithFees -> do 
                                    liftIO $ echo "> Select the utxo containing Ada to transfer  :" 
                                    Wallet.selectUTxOFilterBy utxoContainingOnlyAda senderWallet 
                                        >>= \case  
                                            Nothing -> liftIO $ echo "UTxO containing ONLY Ada not found in your wallet."
                                            Just utxoWithAda  -> do
                                                let (_,_,totalAmount) = getTokenFrom utxoWithAda
                                                amount          <- liftIO $ echo "-n" "> Amount of Ada (in lovelaces) : "   >>  read @Integer <$> getLine
                                                run_tx paymentSigningKeyPath 
                                                        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithAda
                                                        , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                        , "--tx-out" , receiverAddr <> " " <> show amount <> " lovelace"
                                                        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral 
                                                        , "--change-address"  , senderAddr]



getTokenFrom :: UTxO -> (CurrencySymbol,TokenName,Integer)
getTokenFrom UTxO {..} = (head . filter (\(c,_,_) -> c == adaSymbol ) .flattenValue) value -- should contains only ADAs 

utxoContainingOnlyAda :: UTxO -> Bool
utxoContainingOnlyAda UTxO {..} 
    = 1 == (length . filter (\(c,_,_) -> c == adaSymbol ) .flattenValue) value