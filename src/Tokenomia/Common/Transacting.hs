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
    ( TxBuild (..)
    , TxInFromWallet (..)
    , TxInFromScript (..)
    , TxOut (..)
    , Metadata (..)
    , ValiditySlotRange (..)
    , MonetaryAction (..)
    , createMetadataFile
    , build
    , buildAndSubmit
    , buildAndSubmitWithoutCollateral
    , submitAndWait
    , BuiltTx (..)
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
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Data.Coerce
{-# ANN module "HLINT: ignore Use camelCase" #-}


load SearchPath ["echo","cardano-cli","md5sum","mv" ]

data BuiltTx = BuiltTx {oneTxInput :: TxOutRef , txBuiltPath :: FilePath} deriving (Show)

build
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)

    => CollateralAddressRef
    -> FeeAddressRef
    -> TxBuild
    -> m BuiltTx
build collateralRef feeAddressRef txBuild@TxBuild {..} = do
    let childAddressRefs =  
            (Wallet.childAddressRef . walletUTxO <$> inputsFromWallet)
            <> (coerce collateralRef :| [coerce feeAddressRef])

    WalletUTxO {utxo = UTxO {txOutRef = collateral}} 
        <- fetchCollateral (coerce collateralRef) >>= whenNothingThrow WalletWithoutCollateral 

    WalletUTxO {utxo = UTxO {txOutRef = fees }, childAddressRef = feesChildAddressRef } 
        <- selectBiggestStrictlyADAsNotCollateral (coerce feeAddressRef) >>= whenNothingThrow NoADAInWallet 
    

    feesChildAddress <- ChildAddress.address <$> fetchById feesChildAddressRef
    extendedPrivateKeyJSONPaths <-  (fmap .fmap) extendedPrivateKeyJSONPath $ sequence (fetchById <$> (childAddressRefs) )

    txBuiltPath <- build'
                          extendedPrivateKeyJSONPaths
                          (toCardanoCLIOptions txBuild
                                <> [ "--tx-in"  , (T.unpack . toCLI) fees]
                                <> [ "--tx-in-collateral" , (T.unpack . toCLI) collateral]
                                <> [ "--change-address"   , coerce feesChildAddress])
    
    return BuiltTx {oneTxInput = fees,txBuiltPath}



buildAndSubmit
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)

    => CollateralAddressRef
    -> FeeAddressRef
    -> TxBuild
    -> m ()
buildAndSubmit collateralRef feeAddressRef txBuild = do
    builtTx <- build collateralRef feeAddressRef txBuild
    printLn "Submitting Tx" >> submitAndWait builtTx


buildAndSubmitWithoutCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)

    => FeeAddressRef
    -> TxBuild
    -> m ()
buildAndSubmitWithoutCollateral feeAddressRef txBuild@TxBuild {..} = do
    let childAddressRefs =  
            (Wallet.childAddressRef . walletUTxO <$> inputsFromWallet)
            <> (coerce feeAddressRef :| [])

    WalletUTxO {utxo = UTxO {txOutRef = fees }, childAddressRef = feesChildAddressRef } 
        <- selectBiggestStrictlyADAsNotCollateral (coerce feeAddressRef) >>= whenNothingThrow NoADAInWallet 

    feesChildAddress <- ChildAddress.address <$> fetchById feesChildAddressRef
    extendedPrivateKeyJSONPaths <-  (fmap .fmap) extendedPrivateKeyJSONPath $ sequence (fetchById <$> childAddressRefs)
    
    txBuiltPath <- build'
      extendedPrivateKeyJSONPaths
      (toCardanoCLIOptions txBuild
        <> [ "--tx-in"  , (T.unpack . toCLI) fees]
        <> [ "--change-address"   , coerce feesChildAddress])

    printLn "Submitting Tx" >> submitAndWait BuiltTx {oneTxInput = fees,txBuiltPath}

build'
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => NonEmpty FilePath
    -> a
    -> m FilePath
build' extendedPrivateKeyJSONPaths buildTxBody = do
    magicN <- asks magicNumber
    
    randomInt <- liftIO ( abs <$> randomIO :: IO Int )
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx_" <> show randomInt <> ".raw")) <$> getFolderPath Transactions
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
    return signedHashTx

    where requiredSigners :: NonEmpty FilePath -> [String]
          requiredSigners xs = foldMap (\a -> ["--required-signer" ,  a ] ) xs


submitAndWait
    :: ( MonadIO m
       , MonadReader Environment m )
    => BuiltTx
    ->  m ()
submitAndWait BuiltTx {..} = do
    magicN <- asks magicNumber
    liftIO $ cardano_cli "transaction" "submit"
         "--testnet-magic" magicN
         "--tx-file" txBuiltPath
    printLn "Waiting for confirmation..."
    awaitTxCommitted oneTxInput 0
    printLn "\nTx committed into ledger"

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

newtype Metadata = Metadata FilePath deriving (Eq,Show)

data ValiditySlotRange = ValiditySlotRange Slot Slot deriving (Eq,Show)

data TxBuild
        = TxBuild
            { inputsFromScript :: Maybe (NonEmpty TxInFromScript)
            , inputsFromWallet  :: NonEmpty TxInFromWallet
            , outputs :: NonEmpty TxOut
            , validitySlotRangeMaybe :: Maybe ValiditySlotRange
            , metadataMaybe :: Maybe Metadata
            , tokenSupplyChangesMaybe :: Maybe (NonEmpty MonetaryAction)} deriving (Eq,Show)


data MonetaryAction
        = Mint {script :: FilePath , amount :: Value}
        | Burn {script :: FilePath , amount :: Value} deriving (Eq,Show)


data TxInFromScript = FromScript { utxoRef :: TxOutRef, script :: FilePath ,datum :: FilePath,redeemer :: FilePath } deriving (Eq,Show)

newtype TxInFromWallet = FromWallet { walletUTxO :: WalletUTxO} deriving (Eq,Show)

data TxOut = ToScript  { address :: Address , value :: Value, datumHash :: Hash}
           | ToWallet  { address :: Address , value :: Value, datumMaybe :: Maybe FilePath} deriving (Eq,Show)


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
    toCardanoCLIOptions ToWallet {datumMaybe = Nothing , ..}  =
        [ "--tx-out" , coerce address <> "  " <> (T.unpack . toCLI) value]
    toCardanoCLIOptions ToWallet {datumMaybe = Just datum , ..}  =
        [ "--tx-out" , coerce address <> "  " <> (T.unpack . toCLI) value
        , "--tx-out-datum-embed-file", datum ]
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