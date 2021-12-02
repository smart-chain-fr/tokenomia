{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Adapter.Cardano.CLI.Transaction
    ( submit
    , awaitTxCommitted
    , BuildingTxError (..)
    , TxBuild (..)
    , TxIn (..)
    , TxOut (..)
    , Metadata (..)
    , ValiditySlotRange (..)
    , Address (..)
    , Hash (..)
    , MonetaryAction (..)
    , submit'
    , createMetadataFile
    , register_protocol_parameters
    , submit_tx
    , submitCollateral
    , sign_tx
    , doubleSign_tx
    , toCardanoCLIOptions
    ) where


import           Data.Coerce
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (fromString)


import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Concurrent
import           System.Random

import           Shh.Internal

import           Ledger ( TxOutRef (..),Value,Slot (..) ) 

import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Adapter.Cardano.CLI.UTxO as UTxO
import           Tokenomia.Adapter.Cardano.CLI.Serialise (toCLI, fromCLI)
import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath,Folder (..))

import           Tokenomia.Adapter.Cardano.Types
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.CLI

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
    | NoFundsToBeRetrieved
    | AllFundsLocked
    | FundAlreadyRetrieved
    deriving Show

newtype Metadata = Metadata FilePath

data ValiditySlotRange = ValiditySlotRange Slot Slot

data TxBuild 
        = TxBuild 
            { wallet :: Wallet
            , txIns  :: NonEmpty TxIn
            , txOuts :: NonEmpty TxOut
            , validitySlotRangeMaybe :: Maybe ValiditySlotRange
            , metadataMaybe :: Maybe Metadata
            , tokenSupplyChangesMaybe :: Maybe (NonEmpty MonetaryAction)}
        

data  MonetaryAction 
        = Mint {script :: FilePath , amount :: Value}
        | Burn {script :: FilePath , amount :: Value}

data TxIn = FromScript { utxoRef :: TxOutRef, script :: FilePath ,datum :: FilePath,redeemer :: FilePath }
          | FromWallet { utxoRef :: TxOutRef}

data TxOut = ToScript  { address :: Address , value :: Value, datumHash :: Hash}
           | ToWallet  { address :: Address , value :: Value} 


class ToCardanoCLIOptions a where 
   toCardanoCLIOptions :: a -> [String]

instance ToCardanoCLIOptions TxIn  where 
    toCardanoCLIOptions FromScript {..}  = 
        [ "--tx-in"  , (T.unpack . toCLI) utxoRef
        , "--tx-in-script-file" , script
        , "--tx-in-datum-file" , datum
        , "--tx-in-redeemer-file" , redeemer]
    toCardanoCLIOptions FromWallet {..}  = 
        ["--tx-in"  , (T.unpack . toCLI) utxoRef]

instance ToCardanoCLIOptions TxOut  where 
    toCardanoCLIOptions ToScript {..}  = 
        [ "--tx-out" , coerce address <> "  " <> (T.unpack . toCLI) value
        , "--tx-out-datum-hash"  , coerce datumHash]
    toCardanoCLIOptions ToWallet {..}  = 
        [ "--tx-out" , coerce address <> "  " <> (T.unpack . toCLI) value]

instance ToCardanoCLIOptions ValiditySlotRange  where 
    toCardanoCLIOptions (ValiditySlotRange (Slot x) (Slot y))  = 
        [ "--invalid-before" , show x
        , "--invalid-hereafter" , show y]

instance ToCardanoCLIOptions MonetaryAction where 
    toCardanoCLIOptions Mint {..}  = 
        [ "--mint" , (T.unpack . toCLI) amount
        , "--mint-script-file" , script
        , "--mint-redeemer-value",  "[]" ]
    toCardanoCLIOptions Burn {..} = 
        [ "--mint" , "-" <> (T.unpack . toCLI) amount
        , "--mint-script-file" , script
        , "--mint-redeemer-value",  "[]"]   

instance ToCardanoCLIOptions Metadata  where 
    toCardanoCLIOptions (Metadata filepath)  = 
        [ "--metadata-json-file", filepath]


instance (Foldable m, ToCardanoCLIOptions a) =>  ToCardanoCLIOptions (m a) where 
    toCardanoCLIOptions  = foldMap toCardanoCLIOptions

instance ToCardanoCLIOptions TxBuild where 
    toCardanoCLIOptions TxBuild {..} 
     =  toCardanoCLIOptions txIns 
     <> toCardanoCLIOptions txOuts
     <> toCardanoCLIOptions tokenSupplyChangesMaybe
     <> toCardanoCLIOptions validitySlotRangeMaybe
     <> toCardanoCLIOptions metadataMaybe 

submit' 
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError BuildingTxError m)

    => TxBuild
    -> m ()
submit' txBuild@TxBuild {wallet = wallet@Wallet {..}} = do
    collateral  <- txOutRef <$> (fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral) 
    utxoForFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)
    submit 
      paymentSigningKeyPath 
       utxoForFees 
       (toCardanoCLIOptions txBuild
        <> [ "--tx-in"  , (T.unpack . toCLI) utxoForFees]
        <> [ "--tx-in-collateral" , (T.unpack . toCLI) collateral]
        <> [ "--change-address"   , coerce paymentAddress])

submitCollateral 
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError BuildingTxError m)

    => TxBuild
    -> m ()
submitCollateral txBuild@TxBuild {wallet = wallet@Wallet {..}} = do
    utxoForFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)
    submit 
      paymentSigningKeyPath 
       utxoForFees 
       (toCardanoCLIOptions txBuild
        <> [ "--tx-in"  , (T.unpack . toCLI) utxoForFees]
        <> [ "--change-address"   , coerce paymentAddress])

submit
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> TxOutRef
    -> a
    -> m ()
submit privateKeyPath aGivenTxOutRef@TxOutRef{..} buildTxBody = do
    magicN <- asks magicNumber
    randomInt <- liftIO ( abs <$> randomIO :: IO Int )
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx_" <> show txOutRefId <> "_ " <> show randomInt <> ".raw")) <$> getFolderPath Transactions
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

    printLn "Signing Tx"    >> sign_tx   rawHashTx signedHashTx privateKeyPath
    printLn "Submitting Tx" >> submit_tx signedHashTx
    printLn "Waiting for confirmation..."
    awaitTxCommitted aGivenTxOutRef 0
    printLn "\nTx committed into ledger"

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
    => TxOutRef
    -> Int
    -> m ()
awaitTxCommitted aGivenTxOutRef duration = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" 
                                                            "--tx-in" ((T.unpack . toCLI ) aGivenTxOutRef) 
                                                            "--testnet-magic" magicN |> capture)
        >>= \case
            []     -> return ()
            [utxo] -> do
                liftIO $ threadDelay 1000000 -- 1s
                liftIO $ echo "-ne" (take duration (repeat '#')) duration "s\r"
                awaitTxCommitted (txOutRef utxo) (duration + 1)
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

doubleSign_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> m ()
doubleSign_tx body_file outFile signing_key_file1 signing_key_file2 = do
    magicN <- asks magicNumber
    liftIO $ cardano_cli "transaction" "sign"
        "--tx-body-file" body_file
        "--signing-key-file" signing_key_file1
        "--signing-key-file" signing_key_file2
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

    liftIO (echo ("{\"" ++ show randomInt ++ "\":{\"message\":\"" ++ message ++ "\"}}")
        &> (Truncate . fromString) metadataJsonFilepath)
    return metadataJsonFilepath
