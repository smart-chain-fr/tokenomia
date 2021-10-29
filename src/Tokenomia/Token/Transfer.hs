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

import qualified Data.Text as T

import           Control.Monad.Reader
import           Control.Monad.Except

import Shh
    ( load,
      ExecReference(SearchPath) )

import           Ledger.Value
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction 
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI

load SearchPath ["echo"]

type Address = String

transfer 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => m ()
transfer = do
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            liftIO $ echo "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets 
    utxoWithToken <- askUTxOFilterBy containingOneToken wallet >>= whenNothingThrow NoUTxOWithOnlyOneToken        
    amount <- liftIO $ echo "-n" "- Amount of Token to transfer : "   >>  read @Integer <$> getLine
    receiverAddr    <- liftIO $ echo "-n" "- Receiver address : "  >>  getLine
    labelMaybe <- liftIO $ echo "-n" "- Add label to your transaction (leave blank if no) : " >> getLine
                    >>= \case
                        [] -> return Nothing
                        label -> (return . Just) label
    transfer' wallet utxoWithToken receiverAddr amount labelMaybe


type MetadataLabel = String

transfer' 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => Wallet 
    -> UTxO
    -> Address 
    -> Integer
    -> Maybe MetadataLabel    
    -> m ()
transfer' senderWallet@Wallet {paymentAddress = senderAddr,..} utxoWithToken receiverAddr amount labelMaybe = do
    collateral <- fetchCollateral senderWallet >>= whenNothingThrow WalletWithoutCollateral  
    utxoForFees <- selectBiggestStrictlyADAsNotCollateral senderWallet >>= whenNothingThrow NoADAInWallet
    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
    case labelMaybe of 
        Nothing -> 
            submit paymentSigningKeyPath utxoForFees
                [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
                , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees 
                , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalAmount - amount) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral 
                , "--change-address"  , senderAddr]
        Just label -> do
            metadataJsonFilepath <- createMetadataFile label
            submit paymentSigningKeyPath utxoForFees
                [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
                , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees 
                , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalAmount - amount) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral 
                , "--change-address"  , senderAddr
                , "--metadata-json-file", metadataJsonFilepath]   
    