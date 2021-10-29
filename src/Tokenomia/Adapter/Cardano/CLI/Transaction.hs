{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Adapter.Cardano.CLI.Transaction
    ( submit
    , awaitTxCommitted
    , BuildingTxError (..)
    , createMetadataFile
    ) where


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import qualified Data.ByteString.Lazy.Char8 as C


 

import           Control.Monad.Reader
import           Control.Concurrent
import           System.Random
import           Data.String (fromString)

import           Shh.Internal


import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Serialise (toCLI, fromCLI)
import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath,Folder (..))

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","cardano-cli","md5sum","mv" ]


data BuildingTxError 
    = NoWalletRegistered
    | NoWalletWithoutCollateral
    | NoWalletWithCollateral
    | WalletWithoutCollateral
    | AlreadyACollateral UTxO
    | NoADAInWallet
    | NoUTxOWithOnlyOneToken 
    | TryingToBurnTokenWithoutScriptRegistered 
    | NoVestingInProgress
    deriving Show


submit
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> UTxO
    -> a
    -> m ()
submit privateKeyPath utxoWithFees buildTxBody = do
    magicN <- asks magicNumber
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions
    protocolParametersPath <- register_protocol_parameters
    liftIO $ cardano_cli
        "transaction"
        "build"
        "--alonzo-era"
        "--testnet-magic" magicN
        (asArg buildTxBody)
        "--required-signer" privateKeyPath
        "--protocol-params-file" protocolParametersPath
        "--out-file" rawTx

    -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . head <$> liftIO (md5sum rawTx |> captureWords )
    liftIO $ mv rawTx rawHashTx

    liftIO (echo "Signing Tx")    >> sign_tx   rawHashTx signedHashTx privateKeyPath
    liftIO (echo "Submitting Tx") >> submit_tx signedHashTx
    liftIO $ echo "Waiting for confirmation..."
    awaitTxCommitted utxoWithFees 0
    liftIO $ echo "\nTx committed into ledger"

submit_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    ->  m ()
submit_tx f = do
    magicN <- asks magicNumber
    liftIO $ cardano_cli "transaction" "submit"
         "--testnet-magic" magicN
         "--tx-file" f

awaitTxCommitted
    :: ( MonadIO m
       , MonadReader Environment m )
    => UTxO
    -> Int
    -> m ()
awaitTxCommitted utxoWithFees duration = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" 
                                                            "--tx-in" ((T.unpack . toCLI . txOutRef) utxoWithFees) 
                                                            "--testnet-magic" magicN |> capture)
        >>= \case
            []     -> return ()
            [utxo] -> do
                liftIO $ threadDelay 1000000 -- 1s
                liftIO $ echo "-ne" (take duration (repeat '#')) duration "s\r"
                awaitTxCommitted utxo (duration + 1)
            _ -> error ("Unexpected value, it is supposed to return [UTxO] or []") 

sign_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> FilePath
    -> FilePath
    -> m ()
sign_tx body_file outFile signing_key_file = do
    magicN <- asks magicNumber
    liftIO $ cardano_cli "transaction" "sign"
        "--tx-body-file" body_file
        "--signing-key-file" signing_key_file
        "--testnet-magic" magicN
        "--out-file" outFile

register_protocol_parameters
    :: ( MonadIO m
        , MonadReader Environment m )
    => m FilePath
register_protocol_parameters = do
    magicN <- asks magicNumber
    folder <- getFolderPath Parameters
    let filePath =  folder <> "parameters-testnet.json"
    liftIO $ cardano_cli
        "query"
        "protocol-parameters"
        "--testnet-magic" magicN
        "--out-file" filePath
    return filePath

createMetadataFile :: (MonadIO m, MonadReader Environment m) => String -> m FilePath
createMetadataFile message = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let metadataJsonFilepath = tmpFolder <> "metadata-" <> show randomInt <> ".json"
    liftIO $ echo "-n" ("{\"" ++ show randomInt ++ "\":{\"message\":\"" ++ message ++ "\"}}")
        &> (Truncate . fromString) metadataJsonFilepath
    return metadataJsonFilepath
