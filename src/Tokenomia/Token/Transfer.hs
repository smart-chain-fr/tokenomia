{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Token.Transfer 
    ( transfer) where

import qualified Data.Text as T

import Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Ledger.Value
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction 
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu  (ask', askMaybe)

type Address = String

transfer 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => m ()
transfer = do
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets 
    utxoWithToken <- askUTxOFilterBy containingOneToken wallet >>= whenNothingThrow NoUTxOWithOnlyOneToken        
    amount <- ask' @Integer "- Amount of Token to transfer : "
    receiverAddr <- ask' @String "- Receiver address : "
    labelMaybe <- askMaybe @String "- Add label to your transaction (leave blank if no) : " 
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
    