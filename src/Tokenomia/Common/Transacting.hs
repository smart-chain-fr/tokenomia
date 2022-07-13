{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Common.Transacting (
  TxBalance (..),
  TxBuild (..),
  TxInFromWallet (..),
  TxInFromScript (..),
  TxOut (..),
  Metadata (..),
  ValiditySlotRange (..),
  MonetaryAction (..),
  calculateMinRequiredUTxO,
  createMetadataFile,
  build,
  buildAndSubmit,
  submitAndWait,
  waitConfirmation,
  submitWithoutWaitingConfimation,
  mockBuild,
  BuiltTx (..),
  Fees,
) where

import Data.ByteString.Lazy.Char8 qualified as C
import Data.Coerce
import Data.List.NonEmpty as NEL
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Prelude hiding (head)
import Prelude qualified

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Reader
import System.Random

import Shh.Internal

import Ledger (Slot (..), TxOutRef (..), Value)

import Tokenomia.Common.Environment
import Tokenomia.Common.Shell.Console (printLn)

import Tokenomia.Common.Data.Convertible (convert)
import Tokenomia.Common.Folder (Folder (..), getFolderPath)
import Tokenomia.Common.Parser (unsafeParseOnly)
import Tokenomia.Common.Parser.MinRequiredUTxO (minRequiredUTxO)
import Tokenomia.Common.Serialise (fromCLI, toCLI)

import Tokenomia.Wallet.UTxO qualified as Wallet
import Tokenomia.Wallet.WalletUTxO qualified as Wallet

import Ledger.Ada as Ada
import Tokenomia.Common.Address
import Tokenomia.Common.Error
import Tokenomia.Common.Hash
import Tokenomia.Wallet.CLI
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress
import Tokenomia.Wallet.Collateral.Read
import Tokenomia.Wallet.UTxO
import Tokenomia.Wallet.WalletUTxO (WalletUTxO (..))

{-# ANN module "HLINT: ignore Use camelCase" #-}

type Fees = Ada
load SearchPath ["echo", "cardano-cli", "md5sum", "mv", "rm"]

buildAndSubmit ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  TxBalance ->
  Maybe CollateralAddressRef ->
  TxBuild ->
  m ()
buildAndSubmit txBalance collateralMaybe txBuild = do
  builtTx <- build txBalance collateralMaybe txBuild
  printLn "Submitting Tx" >> submitAndWait builtTx

build ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  TxBalance ->
  Maybe CollateralAddressRef ->
  TxBuild ->
  m BuiltTx
build txBalance collateralMaybe txBuild@TxBuild {..} = do
  let childAddressRefs = getTxChildAddressRefs txBalance txBuild collateralMaybe
      firstInputTxOutRef = (txOutRef . utxo . walletUTxO . NEL.head) inputsFromWallet
  extendedPrivateKeyJSONPaths <- fmap extendedPrivateKeyJSONPath <$> sequence (fetchById <$> childAddressRefs)
  collateralOptions <- getCollateralCardanoCLIOptions collateralMaybe
  feesOptions <- getFeesCardanoCLIOptions txBalance
  (txBuiltPath, txSignedPath, estimatedFees) <-
    build'
      txBalance
      extendedPrivateKeyJSONPaths
      (toCardanoCLIOptions txBuild <> collateralOptions <> feesOptions)

  return BuiltTx {oneTxInput = firstInputTxOutRef, txSignedPath, txBuiltPath, estimatedFees}

mockBuild ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  TxBalance ->
  Maybe CollateralAddressRef ->
  TxBuild ->
  m Ada
mockBuild txBalance collateralMaybe txBuild =
  build txBalance collateralMaybe txBuild >>= \BuiltTx {..} -> do
    liftIO $ rm txBuiltPath
    liftIO $ rm txSignedPath
    return estimatedFees

getCollateralCardanoCLIOptions ::
  (MonadIO m, MonadReader Environment m, MonadError TokenomiaError m) =>
  Maybe CollateralAddressRef ->
  m [String]
getCollateralCardanoCLIOptions (Just collateralRef) = do
  WalletUTxO {utxo = UTxO {txOutRef = collateral}} <-
    fetchCollateral (coerce collateralRef) >>= whenNothingThrow WalletWithoutCollateral
  return ["--tx-in-collateral", (T.unpack . toCLI) collateral]
getCollateralCardanoCLIOptions Nothing = return []

getFeesCardanoCLIOptions ::
  (MonadIO m, MonadReader Environment m, MonadError TokenomiaError m) =>
  TxBalance ->
  m [String]
getFeesCardanoCLIOptions Balanced {..} = return ["--fee", show $ getLovelace txFees]
getFeesCardanoCLIOptions (Unbalanced feeAddressRef) = do
  WalletUTxO {utxo = UTxO {txOutRef = fees}, childAddressRef = feesChildAddressRef} <-
    selectBiggestStrictlyADAsNotCollateral (coerce feeAddressRef) >>= whenNothingThrow NoADAsOnChildAddress
  feesChildAddress <- ChildAddress.address <$> fetchById feesChildAddressRef
  magicN <- asks magicNumber
  return $
    ["--tx-in", (T.unpack . toCLI) fees]
      <> ["--change-address", coerce feesChildAddress]
      <> ["--testnet-magic", show magicN]

getTxChildAddressRefs :: TxBalance -> TxBuild -> Maybe CollateralAddressRef -> NonEmpty ChildAddressRef
getTxChildAddressRefs a TxBuild {..} c =
  let childaddressesFromInputWallet = Wallet.childAddressRef . walletUTxO <$> inputsFromWallet
   in case (a, c) of
        (Balanced {}, Nothing) -> childaddressesFromInputWallet
        (Balanced {}, Just collateralRef) -> childaddressesFromInputWallet <> (coerce collateralRef :| [])
        (Unbalanced feeAddressRef, Just collateralRef) -> childaddressesFromInputWallet <> (coerce collateralRef :| [coerce feeAddressRef])
        (Unbalanced feeAddressRef, Nothing) -> childaddressesFromInputWallet <> (coerce feeAddressRef :| [])

data TxBalance = Unbalanced {feeAddressRef :: FeeAddressRef} | Balanced {txFees :: Fees}

getBuildMode :: TxBalance -> String
getBuildMode (Unbalanced _) = "build"
getBuildMode Balanced {} = "build-raw"

calculateMinRequiredUTxO ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  TxOut ->
  m Value
calculateMinRequiredUTxO txOut = do
  protocolParametersPath <- register_protocol_parameters
  result <-
    liftIO $
      cardano_cli
        "transaction"
        "calculate-min-required-utxo"
        "--protocol-params-file"
        protocolParametersPath
        (toCardanoCLIOptions txOut)
        |> capture
  pure $ unsafeParseOnly minRequiredUTxO (convert result)

build' ::
  ( ExecArg a
  , MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  TxBalance ->
  NonEmpty FilePath ->
  a ->
  m (FilePath, FilePath, Ada)
build' buildMode extendedPrivateKeyJSONPaths buildTxBody = do
  randomInt <- liftIO (abs <$> randomIO :: IO Int)
  (txFolder, rawTx) <- (\a -> (a, a <> "tx_" <> show randomInt <> ".raw")) <$> getFolderPath Transactions
  protocolParametersPath <- register_protocol_parameters
  fees <-
    liftIO
      ( cardano_cli
          "transaction"
          (getBuildMode buildMode)
          "--alonzo-era"
          (asArg buildTxBody)
          (asArg $ requiredSigners extendedPrivateKeyJSONPaths)
          "--protocol-params-file"
          protocolParametersPath
          "--out-file"
          rawTx
          |> captureWords
      )
      >>= whenLeftThrow InvalidTransaction . translate buildMode . fmap C.unpack

  (builtTxPath, signedTxPath) <-
    ( \txHash ->
        ( txFolder <> txHash <> ".raw"
        , txFolder <> txHash <> ".signed"
        )
      )
      . C.unpack
      . Prelude.head
      <$> liftIO (md5sum rawTx |> captureWords)
  liftIO $ mv rawTx builtTxPath

  printLn "> Signing Tx" >> sign_tx builtTxPath signedTxPath extendedPrivateKeyJSONPaths
  printLn "> Tx Built and Signed"
  return (builtTxPath, signedTxPath, fees)
  where
    requiredSigners :: NonEmpty FilePath -> [String]
    requiredSigners xs = foldMap (\a -> ["--required-signer", a]) xs

    translate :: TxBalance -> [String] -> Either String Ada
    translate Unbalanced {} ["Estimated", "transaction", "fee:", "Lovelace", feesAsString] = (Right . fromIntegral . read @Integer) feesAsString
    translate Balanced {txFees} [] = Right txFees
    translate _ issue = (Left . unwords) issue

submitWithoutWaitingConfimation ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  BuiltTx ->
  m BuiltTx
submitWithoutWaitingConfimation tx@BuiltTx {..} = do
  magicN <- asks magicNumber
  liftIO $
    cardano_cli
      "transaction"
      "submit"
      "--testnet-magic"
      magicN
      "--tx-file"
      txSignedPath
  printLn "Tx Submitted..."
  return tx

waitConfirmation ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  BuiltTx ->
  m ()
waitConfirmation BuiltTx {..} = do
  printLn "Waiting for confirmation..."
  awaitTxCommitted oneTxInput 0
  printLn "\nTx committed into ledger"

submitAndWait ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  BuiltTx ->
  m ()
submitAndWait BuiltTx {..} = do
  magicN <- asks magicNumber
  liftIO $
    cardano_cli
      "transaction"
      "submit"
      "--testnet-magic"
      magicN
      "--tx-file"
      txSignedPath
  printLn "Waiting for confirmation..."
  awaitTxCommitted oneTxInput 0
  printLn "\nTx committed into ledger"

awaitTxCommitted ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  TxOutRef ->
  Int ->
  m ()
awaitTxCommitted aGivenTxOutRef duration = do
  magicN <- asks magicNumber
  liftIO
    ( cardano_cli
        "query"
        "utxo"
        "--tx-in"
        ((T.unpack . toCLI) aGivenTxOutRef)
        "--testnet-magic"
        magicN
        |> capture
    )
    >>= ( \case
            [] -> return ()
            [utxo] -> do
              liftIO $ threadDelay 1000000 -- 1s
              liftIO $ echo "-ne" (replicate duration '#') duration "s\r"
              awaitTxCommitted (Wallet.txOutRef utxo) (duration + 1)
            _ -> error "Unexpected value, it is supposed to return [UTxO] or []"
        )
      . fromCLI
      . TL.toStrict
      . TLE.decodeUtf8

sign_tx ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  FilePath ->
  FilePath ->
  NonEmpty FilePath ->
  m ()
sign_tx body_file outFile extendedPrivateKeyJSONPaths = do
  magicN <- asks magicNumber
  liftIO $
    cardano_cli
      "transaction"
      "sign"
      "--tx-body-file"
      body_file
      (asArg $ signingKeyFiles extendedPrivateKeyJSONPaths)
      "--testnet-magic"
      magicN
      "--out-file"
      outFile
  where
    signingKeyFiles :: NonEmpty FilePath -> [String]
    signingKeyFiles xs = foldMap (\a -> ["--signing-key-file", a]) xs

register_protocol_parameters ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  m FilePath
register_protocol_parameters = do
  magicN <- asks magicNumber
  folder <- getFolderPath Parameters
  let filePath = folder <> "parameters-testnet.json"
  liftIO $
    cardano_cli
      "query"
      "protocol-parameters"
      "--testnet-magic"
      magicN
      "--out-file"
      filePath
  return filePath

createMetadataFile ::
  (MonadIO m, MonadReader Environment m) =>
  String ->
  m FilePath
createMetadataFile message = do
  tmpFolder <- getFolderPath TMP
  randomInt <- liftIO (abs <$> randomIO :: IO Integer)
  let metadataJsonFilepath = tmpFolder <> "metadata-" <> show randomInt <> ".json"

  liftIO
    ( echo ("{\"" ++ show randomInt ++ "\":{\"message\":\"" ++ message ++ "\"}}")
        &> (Truncate . fromString) metadataJsonFilepath
    )
  return metadataJsonFilepath

data BuiltTx = BuiltTx
  { oneTxInput :: TxOutRef
  , txBuiltPath :: FilePath
  , txSignedPath :: FilePath
  , estimatedFees :: Ada
  }
  deriving stock (Show)

newtype Metadata = Metadata FilePath deriving stock (Eq, Show)

data ValiditySlotRange = ValiditySlotRange Slot Slot deriving stock (Eq, Show)

data TxBuild = TxBuild
  { inputsFromScript :: Maybe (NonEmpty TxInFromScript)
  , inputsFromWallet :: NonEmpty TxInFromWallet
  , outputs :: NonEmpty TxOut
  , validitySlotRangeMaybe :: Maybe ValiditySlotRange
  , metadataMaybe :: Maybe Metadata
  , tokenSupplyChangesMaybe :: Maybe (NonEmpty MonetaryAction)
  }
  deriving stock (Eq, Show)

data MonetaryAction
  = Mint {script :: FilePath, amount :: Value}
  | Burn {script :: FilePath, amount :: Value}
  deriving stock (Eq, Show)

data TxInFromScript = FromScript {utxoRef :: TxOutRef, script :: FilePath, datum :: FilePath, redeemer :: FilePath} deriving stock (Eq, Show)

newtype TxInFromWallet = FromWallet {walletUTxO :: WalletUTxO} deriving stock (Eq, Show)

data TxOut
  = ToScript {address :: Address, value :: Value, datumHash :: Hash}
  | ToWallet {address :: Address, value :: Value, datumMaybe :: Maybe FilePath}
  deriving stock (Eq, Show)

class ToCardanoCLIOptions a where
  toCardanoCLIOptions :: a -> [String]

instance ToCardanoCLIOptions TxInFromScript where
  toCardanoCLIOptions FromScript {..} =
    [ "--tx-in"
    , (T.unpack . toCLI) utxoRef
    , "--tx-in-script-file"
    , script
    , "--tx-in-datum-file"
    , datum
    , "--tx-in-redeemer-file"
    , redeemer
    ]

instance ToCardanoCLIOptions TxInFromWallet where
  toCardanoCLIOptions FromWallet {walletUTxO = WalletUTxO {utxo = UTxO {..}}} =
    ["--tx-in", (T.unpack . toCLI) txOutRef]

instance ToCardanoCLIOptions TxOut where
  toCardanoCLIOptions ToScript {..} =
    [ "--tx-out"
    , coerce address <> "  " <> (T.unpack . toCLI) value
    , "--tx-out-datum-hash"
    , coerce datumHash
    ]
  toCardanoCLIOptions ToWallet {datumMaybe = Nothing, ..} =
    ["--tx-out", coerce address <> "  " <> (T.unpack . toCLI) value]
  toCardanoCLIOptions ToWallet {datumMaybe = Just datum, ..} =
    [ "--tx-out"
    , coerce address <> "  " <> (T.unpack . toCLI) value
    , "--tx-out-datum-embed-file"
    , datum
    ]
instance ToCardanoCLIOptions ValiditySlotRange where
  toCardanoCLIOptions (ValiditySlotRange (Slot x) (Slot y)) =
    [ "--invalid-before"
    , show x
    , "--invalid-hereafter"
    , show y
    ]

instance ToCardanoCLIOptions MonetaryAction where
  toCardanoCLIOptions Mint {..} =
    [ "--mint"
    , (T.unpack . toCLI) amount
    , "--mint-script-file"
    , script
    , "--mint-redeemer-value"
    , "[]"
    ]
  toCardanoCLIOptions Burn {..} =
    [ "--mint"
    , "-" <> (T.unpack . toCLI) amount
    , "--mint-script-file"
    , script
    , "--mint-redeemer-value"
    , "[]"
    ]

instance ToCardanoCLIOptions Metadata where
  toCardanoCLIOptions (Metadata filepath) =
    ["--metadata-json-file", filepath]

instance (Foldable m, ToCardanoCLIOptions a) => ToCardanoCLIOptions (m a) where
  toCardanoCLIOptions = foldMap toCardanoCLIOptions

instance ToCardanoCLIOptions TxBuild where
  toCardanoCLIOptions TxBuild {..} =
    toCardanoCLIOptions inputsFromWallet
      <> toCardanoCLIOptions inputsFromScript
      <> toCardanoCLIOptions outputs
      <> toCardanoCLIOptions tokenSupplyChangesMaybe
      <> toCardanoCLIOptions validitySlotRangeMaybe
      <> toCardanoCLIOptions metadataMaybe
