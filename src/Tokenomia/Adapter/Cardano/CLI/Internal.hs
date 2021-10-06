{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}

{- | It's a simple wrapper around the cardano-cli with Shh library. Waiting for the PAB to be available 
     on the Testnet and the Mainnet, Whe have a need for a complex sequences of transactions. 
     To be less error prone we have chosen this alternative over using bash scripts for example.
     It allows you to use a part of your Off chain codebase in Haskell basically. 
-}
module Tokenomia.Adapter.Cardano.CLI.Internal
    ( -- Write 
      run_tx
    , register_minting_script_file
    , get_monetary_policy_path
    , register_shelley_wallet
    , remove_shelley_wallet
    , recover_from_seed_phrase
      -- Read 
    , query_registered_wallets
    , query_utxo
    , query_tip
    , Wallet (..)
    , WalletAddress
    , Environment (..)) where

import Shh.Internal
    ( ExecArg(asArg),
      captureWords,
      load,
      (|>),
      (&>),
      Stream(Truncate),
      ExecReference(SearchPath), capture )

import           Tokenomia.Common.Shell.InteractiveMenu
import           Control.Monad.Reader
import           Data.String

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           System.Environment (getEnv)
import           Ledger.Contexts ( scriptCurrencySymbol )
import           Codec.Serialise ( serialise )

import           Ledger ( unMintingPolicyScript, MintingPolicy, CurrencySymbol )

import           Cardano.Api ( writeFileTextEnvelope, Error(displayError) )
import qualified Cardano.Api.Shelley  as Script
import           System.Directory


{-# ANN module "HLINT: ignore Use camelCase" #-}


load SearchPath ["cat","echo","mkdir","md5sum","mv","cardano-cli","awk","ls", "rm", "cardano-address" ]


type TxOutRef = String
type WalletAddress = String
type WalletName = String
type PaymentAddress = String

data Environment = Testnet {magicNumber :: Integer}

data Wallet = Wallet
              { name :: WalletName
              , paymentAddress :: PaymentAddress
              , paymentSigningKeyPath :: FilePath }

instance Show Wallet where
    show Wallet {..} = ">> " <> name <> " :" <> paymentAddress

instance DisplayMenuItem Wallet where
    displayMenuItem Wallet {..} = name


query_registered_wallets :: (MonadIO m ) => m [Wallet]
query_registered_wallets = do
   keyPath <- liftIO $ getFolderPath Keys
   walletNames <- liftIO $ (fmap.fmap) C.unpack (ls keyPath |> captureWords)
   mapM (\name ->
        do
        let paymentAddressPath = keyPath <> name <> "/payment.addr"
            paymentSigningKeyPath = keyPath <> name <> "/payment-signing.skey"
        paymentAddress <- liftIO $ C.unpack  <$> (cat paymentAddressPath |> capture)
        return $ Wallet {..} ) walletNames

generate_seed_phrase
    :: ( MonadIO m )
    => WalletName
    -> m ()
generate_seed_phrase walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
    liftIO $ cardano_address "recovery-phrase" "generate" "--size" "24"
        &> (Truncate . fromString) mnemonics


register_shelley_wallet
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName
    -> m ()
register_shelley_wallet walletName = do
    generate_seed_phrase walletName
    generate_keys walletName

recover_from_seed_phrase
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName -> String
    -> m ()
recover_from_seed_phrase walletName seedPhrase = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
    liftIO $ mkdir "-p" walletKeyPath
    liftIO $ echo seedPhrase
        &> (Truncate . fromString) mnemonics
    generate_keys walletName

generate_keys
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName
    -> m ()
generate_keys walletName = do
    Testnet {..} <- ask
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
        root = walletKeyPath <> "root.xsk"
        paymentSigning = walletKeyPath <> "payment-signing.xsk"
        paymentVerification = walletKeyPath <> "payment-verification.xvk"
        stakeVerification = walletKeyPath <> "stake.xvk"
        shortPaymentAddress = walletKeyPath <> "payment.addr"

    liftIO $ mkdir "-p" walletKeyPath
    liftIO $ (cat mnemonics |> cardano_address "key" "from-recovery-phrase" "Shelley")
        &> (Truncate . fromString) root
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/0/0")
        &> (Truncate . fromString) paymentSigning
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/0/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) paymentVerification
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/2/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) stakeVerification
    liftIO $ (cat paymentVerification |> cardano_address "address" "payment" "--network-tag" "testnet")
        &> (Truncate . fromString) shortPaymentAddress
    convertKeys walletName

convertKeys
    :: ( MonadIO m )
    => WalletName
    -> m ()
convertKeys walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        paymentSigningConverted = walletKeyPath <> "payment-signing.skey"
        paymentSigning = walletKeyPath <> "payment-signing.xsk"
        paymentVerificationConverted = walletKeyPath <> "payment-verification.vkey"

    liftIO $ cardano_cli "key" "convert-cardano-address-key" "--shelley-payment-key" "--signing-key-file" paymentSigning
        "--out-file" paymentSigningConverted
    liftIO $ cardano_cli "key" "verification-key" "--signing-key-file" paymentSigningConverted "--verification-key-file"
        paymentVerificationConverted



remove_shelley_wallet :: WalletName -> IO ()
remove_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"

    rm "-rf" walletKeyPath


query_tip
    :: ( MonadIO m
       , MonadReader Environment m )
    => m T.Text
query_tip = do
    Testnet {..} <- ask
    (TL.toStrict . TLE.decodeUtf8) <$> liftIO (cardano_cli "query" "tip" "--testnet-magic" magicNumber |> capture)

query_utxo
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletAddress
    -> m T.Text
query_utxo walletAddress = do
    Testnet {..} <- ask
    (TL.toStrict . TLE.decodeUtf8) <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicNumber "--address" walletAddress |> capture)

-- | Build a Tx , Sign it with the private key path provided and Submit it
--   Temporary Files are persisted into ~/.cardano-cli/ folder 
run_tx
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> a
    -> m ()
run_tx privateKeyPath buildTxBody = do
    Testnet {..} <- ask
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions
    protocolParametersPath <- register_protocol_parameters
    liftIO $ cardano_cli
        "transaction"
        "build"
        "--alonzo-era"
        "--testnet-magic" magicNumber
        (asArg buildTxBody)
        "--protocol-params-file" protocolParametersPath
        "--out-file" rawTx

    -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . head <$> (liftIO $ (md5sum rawTx |> captureWords ))
    liftIO $ mv rawTx rawHashTx

    (liftIO $ echo "Signing Tx")    >> sign_tx   rawHashTx signedHashTx privateKeyPath
    (liftIO $ echo "Submitting Tx") >> submit_tx signedHashTx
    liftIO $ echo "Tx sent"

submit_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    ->  m ()
submit_tx f = do
    Testnet {..} <- ask
    liftIO $ cardano_cli "transaction" "submit"
         "--testnet-magic" magicNumber
         "--tx-file" f

sign_tx
    :: ( MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> FilePath
    -> FilePath
    -> m ()
sign_tx body_file outFile signing_key_file = do
    Testnet {..} <- ask
    liftIO $ cardano_cli "transaction" "sign"
        "--tx-body-file" body_file
        "--signing-key-file" signing_key_file
        "--testnet-magic" magicNumber
        "--out-file" outFile

register_protocol_parameters
    :: ( MonadIO m
        , MonadReader Environment m )
    => m FilePath
register_protocol_parameters = do
    Testnet {..} <- ask
    folder <- getFolderPath Parameters
    let filePath =  folder <> "parameters-testnet.json"
    liftIO $ cardano_cli
        "query"
        "protocol-parameters"
        "--testnet-magic" magicNumber
        "--out-file" filePath
    return filePath


get_monetary_policy_path
    :: ( MonadIO m )
    => CurrencySymbol
    -> m (Maybe FilePath)
get_monetary_policy_path  currencySymbol = do
    txFolder <- getFolderPath Transactions
    let monetaryFilePath = txFolder <> show currencySymbol <> ".plutus"
    liftIO (doesFileExist monetaryFilePath)
        >>= \case
             True ->  (return . Just) monetaryFilePath
             False -> return Nothing


register_minting_script_file
    :: ( MonadIO m )
    => MintingPolicy
    -> m FilePath
register_minting_script_file mp = do
    txFolder <- getFolderPath Transactions
    let filePath =  txFolder <> show (scriptCurrencySymbol mp) <> ".plutus"
    liftIO $ writeFileTextEnvelope filePath Nothing (toPlutusScriptV1 mp)
        >>= (\case
            Left err -> error $ displayError err
            Right () -> return filePath)


data Folder = Transactions | Keys | Parameters

getFolderPath :: (MonadIO m) => Folder -> m FilePath
getFolderPath folder
    =  getFolderPath'
            $ case folder of
                Transactions -> "transactions"
                Keys ->  "keys"
                Parameters -> "parameters"

getFolderPath' :: (MonadIO m) => String -> m FilePath
getFolderPath' s = do
    a <- ( <> "/"<> s<>"/") <$> getRootCLIFolder
    liftIO $ mkdir "-p" a
    return a

getRootCLIFolder :: (MonadIO m) => m FilePath
getRootCLIFolder = do
    a <- ( <> "/.tokenomia-cli/") <$> (liftIO . getEnv) "HOME"
    liftIO $ mkdir "-p" a
    return a

toPlutusScriptV1 :: MintingPolicy -> Script.PlutusScript Script.PlutusScriptV1
toPlutusScriptV1
  = Script.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . unMintingPolicyScript
                  