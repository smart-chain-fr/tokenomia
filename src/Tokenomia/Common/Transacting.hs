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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Common.Transacting
    ( awaitTxCommitted
    , TxBuild (..)
    , TxInFromWallet (..)
    , TxInFromScript (..)
    , TxOut (..)
    , Metadata (..)
    , ValiditySlotRange (..)
    , MonetaryAction (..)
    , submit
    , createMetadataFile
    , register_protocol_parameters
    , submit_tx
    , submitCollateral
    , sign_tx
    , toCardanoCLIOptions
    ) where

import Prelude hiding (head)
import qualified Prelude
import           Data.Coerce
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List.NonEmpty hiding (take,repeat)
import           Data.String (fromString)


import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Concurrent
import           System.Random

import           Shh.Internal

import           Ledger ( TxOutRef (..),Value,Slot (..) )

import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Environment


import           Tokenomia.Common.Serialise (toCLI, fromCLI)
import           Tokenomia.Common.Folder (getFolderPath,Folder (..))



import qualified Tokenomia.Wallet.UTxO as Wallet


import           Tokenomia.Common.Address
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.CLI
import           Tokenomia.Wallet.UTxO
import           Tokenomia.Common.Hash   
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress

{-# ANN module "HLINT: ignore Use camelCase" #-}


load SearchPath ["echo","cardano-cli","md5sum","mv" ]

newtype Metadata = Metadata FilePath

data ValiditySlotRange = ValiditySlotRange Slot Slot

data TxBuild
        = TxBuild
            { inputsFromScript :: Maybe (NonEmpty TxInFromScript)
            , inputsFromWallet  :: NonEmpty TxInFromWallet
            , outputs :: NonEmpty TxOut
            , validitySlotRangeMaybe :: Maybe ValiditySlotRange
            , metadataMaybe :: Maybe Metadata
            , tokenSupplyChangesMaybe :: Maybe (NonEmpty MonetaryAction)}


data MonetaryAction
        = Mint {script :: FilePath , amount :: Value}
        | Burn {script :: FilePath , amount :: Value}


data TxInFromScript = FromScript { utxoRef :: TxOutRef, script :: FilePath ,datum :: FilePath,redeemer :: FilePath }

newtype TxInFromWallet = FromWallet { walletUTxO :: WalletUTxO}

data TxOut = ToScript  { address :: Address , value :: Value, datumHash :: Hash}
           | ToWallet  { address :: Address , value :: Value}


class ToCardanoCLIOptions a where
   toCardanoCLIOptions :: a -> [String]

instance ToCardanoCLIOptions TxInFromScript  where
    toCardanoCLIOptions FromScript {..}  =
        [ "--tx-in"  , (T.unpack . toCLI) utxoRef
        , "--tx-in-script-file" , script
        , "--tx-in-datum-file" , datum
        , "--tx-in-redeemer-file" , redeemer]

instance ToCardanoCLIOptions TxInFromWallet  where
    toCardanoCLIOptions FromWallet {walletUTxO = WalletUTxO {utxo = UTxO  {..}}}  =
        ["--tx-in"  , (T.unpack . toCLI) txOutRef]

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
     =  toCardanoCLIOptions inputsFromWallet
     <> toCardanoCLIOptions inputsFromScript
     <> toCardanoCLIOptions outputs
     <> toCardanoCLIOptions tokenSupplyChangesMaybe
     <> toCardanoCLIOptions validitySlotRangeMaybe
     <> toCardanoCLIOptions metadataMaybe

submit
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)

    => TxBuild
    -> m ()
submit txBuild@TxBuild {..} = do
    let childAddressRefs =  childAddressRef . walletUTxO <$> inputsFromWallet

    
    WalletUTxO {utxo = UTxO {txOutRef = collateral}} :| _ :: NonEmpty WalletUTxO
        <- sequence (fetchCollateral <$> childAddressRefs) >>= whenNothingThrow WalletWithoutCollateral . sequence

    biggestUtxosFromChildAddresses <- sequence (selectBiggestStrictlyADAsNotCollateral <$> childAddressRefs) >>= whenNothingThrow NoADAInWallet . sequence
    let WalletUTxO {utxo = UTxO {txOutRef = fees }, childAddressRef = feesChildAddressRef } :: WalletUTxO = head biggestUtxosFromChildAddresses

    feesChildAddress <- ChildAddress.address <$> fetchById feesChildAddressRef
    extendedPrivateKeyJSONPaths <-  (fmap .fmap) extendedPrivateKeyJSONPath $ sequence (fetchById <$> childAddressRefs)

    submit'
      extendedPrivateKeyJSONPaths
       fees
       (toCardanoCLIOptions txBuild
        <> [ "--tx-in"  , (T.unpack . toCLI) fees]
        <> [ "--tx-in-collateral" , (T.unpack . toCLI) collateral]
        <> [ "--change-address"   , coerce feesChildAddress])

submitCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)

    => TxBuild
    -> m ()
submitCollateral txBuild@TxBuild {..} = do
    let childAddressRefs =  childAddressRef . walletUTxO <$> inputsFromWallet

    biggestUtxosFromChildAddresses <- sequence (selectBiggestStrictlyADAsNotCollateral <$> childAddressRefs) >>= whenNothingThrow NoADAInWallet . sequence
    let WalletUTxO {utxo = UTxO {txOutRef = fees }, childAddressRef = feesChildAddressRef } :: WalletUTxO =  head biggestUtxosFromChildAddresses

    feesChildAddress <- ChildAddress.address <$> fetchById feesChildAddressRef
    extendedPrivateKeyJSONPaths <-  (fmap .fmap) extendedPrivateKeyJSONPath $ sequence (fetchById <$> childAddressRefs)
    
    submit'
      extendedPrivateKeyJSONPaths
       fees
       (toCardanoCLIOptions txBuild
        <> [ "--tx-in"  , (T.unpack . toCLI) fees]
        <> [ "--change-address"   , coerce feesChildAddress])

submit'
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => NonEmpty FilePath
    -> TxOutRef
    -> a
    -> m ()
submit' extendedPrivateKeyJSONPaths aGivenTxOutRef@TxOutRef{..} buildTxBody = do
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
        (asArg $ requiredSigners extendedPrivateKeyJSONPaths)
        "--protocol-params-file" protocolParametersPath
        "--out-file" rawTx

    -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . Prelude.head <$> liftIO (md5sum rawTx |> captureWords )
    liftIO $ mv rawTx rawHashTx

    printLn "Signing Tx"    >> sign_tx   rawHashTx signedHashTx extendedPrivateKeyJSONPaths
    printLn "Submitting Tx" >> submit_tx signedHashTx
    printLn "Waiting for confirmation..."
    awaitTxCommitted aGivenTxOutRef 0
    printLn "\nTx committed into ledger"

    where requiredSigners :: NonEmpty FilePath -> [String]
          requiredSigners xs = foldMap (\a -> ["--required-signer" ,  a ] ) xs

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
    liftIO (cardano_cli 
                "query" 
                "utxo"
                "--tx-in" ((T.unpack . toCLI  ) aGivenTxOutRef)
                "--testnet-magic" magicN |> capture) 
            >>= (\case
                    []     -> return ()
                    [utxo] -> do
                        liftIO $ threadDelay 1000000 -- 1s
                        liftIO $ echo "-ne" (replicate duration '#') duration "s\r"
                        awaitTxCommitted (Wallet.txOutRef utxo) (duration + 1)
                    _ -> error "Unexpected value, it is supposed to return [UTxO] or []") . fromCLI . TL.toStrict . TLE.decodeUtf8

sign_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> FilePath
    -> NonEmpty FilePath
    -> m ()
sign_tx body_file outFile extendedPrivateKeyJSONPaths = do
    magicN <- asks magicNumber
    liftIO $ cardano_cli "transaction" "sign"
                "--tx-body-file" body_file
                (asArg $ signingKeyFiles extendedPrivateKeyJSONPaths)
                "--testnet-magic" magicN
                "--out-file" outFile
    where signingKeyFiles :: NonEmpty FilePath -> [String]
          signingKeyFiles xs = foldMap (\a -> ["--signing-key-file" ,  a ] ) xs


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
