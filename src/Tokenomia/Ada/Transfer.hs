{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Ada.Transfer
    ( transfer ) where

import qualified Data.Text as T
import           Control.Monad.Reader
import           Control.Monad.Except
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction


import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask', askMaybe)


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
    utxo <- selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet
    printLn $ "- Amount Available : " <> showValue (value utxo)

    amount <- ask' @Integer "- Amount of Lovelaces to transfer : "
    receiverAddr <- ask' @String "- Receiver address : "
    labelMaybe <- askMaybe @String "- Add label to your transaction (leave blank if no) : "
    transfer' wallet  receiverAddr amount labelMaybe


type MetadataLabel = String

transfer'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => Wallet
    -> Address
    -> Integer
    -> Maybe MetadataLabel 
    -> m ()
transfer' senderWallet@Wallet {paymentAddress = senderAddr,..} receiverAddr amount labelMaybe = do
    collateral <- fetchCollateral senderWallet >>= whenNothingThrow WalletWithoutCollateral
    utxoForADAandFees <- selectBiggestStrictlyADAsNotCollateral senderWallet >>= whenNothingThrow NoADAInWallet
    case labelMaybe of 
        Nothing -> 
            submit paymentSigningKeyPath utxoForADAandFees
                    [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForADAandFees
                    , "--tx-out" , receiverAddr <> " " <> show amount <> " lovelace"
                    , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
                    , "--change-address"  , senderAddr]
        Just label -> do
            metadataJsonFilepath <- createMetadataFile label
            submit paymentSigningKeyPath utxoForADAandFees
                    [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForADAandFees
                    , "--tx-out" , receiverAddr <> " " <> show amount <> " lovelace"
                    , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
                    , "--change-address"  , senderAddr
                    , "--metadata-json-file", metadataJsonFilepath]        

