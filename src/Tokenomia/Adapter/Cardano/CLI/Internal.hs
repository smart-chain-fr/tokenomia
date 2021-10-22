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

{- | It's a simple wrapper around the cardano-cli with Shh library. Waiting for the PAB to be available 
     on the Testnet and the Mainnet, Whe have a need for a complex sequences of transactions. 
     To be less error prone we have chosen this alternative over using bash scripts for example.
     It allows you to use a part of your Off chain codebase in Haskell basically. 
-}
module Tokenomia.Adapter.Cardano.CLI.Internal
    ( -- Write 
      submitTx
    , registerMintingScriptFile
    , registerValidatorScriptFile
    , registerVestingIndex
    -- Wallet
    , register_shelley_wallet
    , remove_shelley_wallet
    , restore_from_seed_phrase
    , query_registered_wallets
    , Wallet (..)
      -- Read 
    , getScriptLocation
    , ScriptLocation (..)
    , getVestingIndex
    , getMonetaryPolicyPath
    , query_utxo
    , getDataHash
    , persistDataInTMP
    , getCurrentSlotSynced
    , getTestnetEnvironmment
    , Environment (..)
    , Address) where

import           Data.Aeson
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as BLU 

import           Control.Monad.Reader
import           Control.Concurrent

import           System.Random
import           System.Directory
import           System.Environment (getEnv)
import           Shh.Internal

import           Codec.Serialise ( serialise )

import           Cardano.Api hiding (Testnet,Mainnet,Address)
import qualified Cardano.Api.Shelley  as Shelley



import           Ledger.Crypto

import           Ledger hiding (Address)

import qualified Plutus.V1.Ledger.Scripts as Script
import           PlutusTx.IsData.Class ( ToData )

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Vesting.Contract
import           Tokenomia.Common.Shell.InteractiveMenu
import           Tokenomia.Adapter.Cardano.CLI.Data (dataToJSONString)
import qualified Tokenomia.Adapter.Cardano.CLI.UTxO as U
import           Tokenomia.Adapter.Cardano.CLI.Serialise (toCLI, fromCLI)

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["cat","echo","mkdir","md5sum","mv","cardano-cli","awk","ls", "rm", "cardano-address" ]


type TxOutRef = String
type WalletName = String
type PaymentAddress = String
type Address = String

data Wallet = Wallet
              { name :: WalletName
              , paymentAddress :: PaymentAddress
              , paymentSigningKeyPath :: FilePath
              , publicKeyHash :: PubKeyHash  }

instance Show Wallet where
    show Wallet {..} = ">> " <> name
        <> " \n public key hash :" <> show publicKeyHash
        <> " \n payment addr :" <> paymentAddress

instance DisplayMenuItem Wallet where
    displayMenuItem Wallet {..} = name


query_registered_wallets :: (MonadIO m, MonadReader Environment m) => m [Wallet]
query_registered_wallets = do
   keyPath <- getFolderPath Keys
   walletNames <- liftIO $ (fmap.fmap) C.unpack (ls keyPath |> captureWords)
   mapM (\name ->
        do
        let paymentAddressPath = keyPath <> name <> "/payment.addr"
            paymentSigningKeyPath = keyPath <> name <> "/payment-signing.skey"
            publickeyPath = keyPath <> name <> "/public-key.hash"
        paymentAddress <- liftIO $ C.unpack  <$> (cat paymentAddressPath |> capture)
        publicKeyHash <- liftIO $ fromString . BLU.toString <$> (cat publickeyPath |> captureTrim)
        return $ Wallet {..} ) walletNames



generate_seed_phrase
    :: ( MonadIO m, MonadReader Environment m )
    => WalletName
    -> m ()
generate_seed_phrase walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
    liftIO $ mkdir "-p" walletKeyPath
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

restore_from_seed_phrase
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName -> String
    -> m ()
restore_from_seed_phrase walletName seedPhrase = do
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

    netWorkTag <- asks (\case
                        Testnet {} -> "testnet"
                        Mainnet {} -> "mainnet")
    liftIO $ (cat paymentVerification |> cardano_address "address" "payment" "--network-tag" netWorkTag)
        &> (Truncate . fromString) shortPaymentAddress
    convertKeys walletName

convertKeys
    :: ( MonadIO m, MonadReader Environment m )
    => WalletName
    -> m ()
convertKeys walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        paymentSigningConvertedPath = walletKeyPath <> "payment-signing.skey"
        paymentSigningPath = walletKeyPath <> "payment-signing.xsk"
        paymentVerificationConvertedPath = walletKeyPath <> "payment-verification.vkey"
        publickeyPath = walletKeyPath <> "public-key.hash"

    liftIO $ cardano_cli "key" "convert-cardano-address-key" "--shelley-payment-key" "--signing-key-file" paymentSigningPath "--out-file" paymentSigningConvertedPath
    liftIO $ cardano_cli "key" "verification-key" "--signing-key-file" paymentSigningConvertedPath "--verification-key-file" paymentVerificationConvertedPath
    liftIO $ cardano_cli "address" "key-hash" "--payment-verification-key-file" paymentVerificationConvertedPath &> (Truncate . fromString) publickeyPath

remove_shelley_wallet :: ( MonadIO m, MonadReader Environment m) =>WalletName -> m ()
remove_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
    liftIO $ rm "-rf" walletKeyPath

getCurrentSlotSynced
    :: ( MonadIO m
       , MonadReader Environment m )
    => m Slot
getCurrentSlotSynced = do
    nodeInfo <- asks localNodeConnectInfo
    liftIO (getLocalChainTip nodeInfo)
        >>= \case
            ChainTipAtGenesis -> error "Got ChainTipAtGenesis as a last Slot..."
            ChainTip (SlotNo slot) _ _ -> (return . Slot . fromIntegral ) slot

query_utxo
    :: ( MonadIO m
       , MonadReader Environment m )
    => Address
    -> m T.Text
query_utxo walletAddress = do
    magicN <- asks magicNumber
    TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" walletAddress |> capture)

-- | Build a Tx , Sign it with the private key path provided and Submit it
--   Temporary Files are persisted into ~/.cardano-cli/ folder 
submitTx
    :: ( ExecArg a
       , MonadIO m
       , MonadReader Environment m )
    => FilePath
    -> U.UTxO
    -> a
    -> m ()
submitTx privateKeyPath utxoWithFees buildTxBody = do
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
    => U.UTxO
    -> Int
    -> m ()
awaitTxCommitted utxoWithFees duration = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" 
                                                            "--tx-in" ((T.unpack . toCLI . U.txOutRef) utxoWithFees) 
                                                            "--testnet-magic" magicN |> capture)
        >>= \case
            []     -> return ()
            [utxo] -> do
                liftIO $ threadDelay 1000000
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


getMonetaryPolicyPath
    :: ( MonadIO m, MonadReader Environment m )
    => CurrencySymbol
    -> m (Maybe FilePath)
getMonetaryPolicyPath  currencySymbol = do
    scFolder <- getFolderPath MonetaryPolicies
    let monetaryFilePath = scFolder <> show currencySymbol <> ".plutus"
    liftIO (doesFileExist monetaryFilePath)
        >>= \case
             True ->  (return . Just) monetaryFilePath
             False -> return Nothing


registerMintingScriptFile
    :: ( MonadIO m, MonadReader Environment m )
    => MintingPolicy
    -> m FilePath
registerMintingScriptFile mp = do
    scFolder <- getFolderPath MonetaryPolicies
    let filePath =  scFolder <> show (scriptCurrencySymbol mp) <> ".plutus"
    liftIO $ writeFileTextEnvelope filePath Nothing ((toPlutusScriptV1 . unMintingPolicyScript) mp)
        >>= (\case
            Left err -> error $ displayError err
            Right () -> return filePath)



registerVestingIndex :: ( MonadIO m , MonadReader Environment m) => VestingParams -> m ()
registerVestingIndex vestingParams = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> liftIO $ encodeFile filePath [vestingParams]
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                        >>= \case
                            Nothing -> error "Vesting index is badly formed"
                            Just params -> liftIO $ encodeFile filePath (vestingParams:params)


getVestingIndex :: ( MonadIO m , MonadReader Environment m) => m [VestingParams]
getVestingIndex = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> return []
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                >>= \case
                    Nothing -> error "Vesting index is badly formed"
                    Just params -> return params


data ScriptLocation = ScriptLocation {onChain :: Address , offChain::FilePath} deriving (Eq,Show)

getScriptLocation :: ( MonadIO m , MonadReader Environment m )
    => Validator
    -> m ScriptLocation
getScriptLocation validator = do
    networkOption <- asks (\case 
                        Mainnet {} -> asArg ["--mainnet"]
                        Testnet {magicNumber} ->  asArg ["--testnet-magic", show magicNumber])
    scFolder <- getFolderPath Validators
    let validatorPath =  scFolder <> show (validatorHash validator) <> ".plutus"
    scriptAddr <- liftIO $ C.unpack  <$> (cardano_cli "address" "build" "--payment-script-file" validatorPath networkOption |> capture)
    return ScriptLocation {onChain = scriptAddr, offChain = validatorPath}

registerValidatorScriptFile
    :: ( MonadIO m , MonadReader Environment m )
    => Validator
    -> m ScriptLocation
registerValidatorScriptFile validator  = do
    scFolder <- getFolderPath Validators
    let validatorPath =  scFolder <> show (validatorHash validator) <> ".plutus"
    liftIO (writeFileTextEnvelope validatorPath Nothing ((toPlutusScriptV1 . unValidatorScript) validator))
        >>= \case
            Left err -> error $ displayError err
            Right () -> getScriptLocation validator





data Folder = Transactions | Keys | Parameters | MonetaryPolicies | Validators | TMP

getFolderPath :: (MonadIO m, MonadReader Environment m) => Folder -> m FilePath
getFolderPath folder
    =  getFolderPath'
            $ case folder of
                Transactions -> "transactions"
                Keys ->  "keys"
                Parameters -> "parameters"
                MonetaryPolicies -> "monetary-policies"
                Validators -> "validators"
                TMP -> "tmp"


getFolderPath' :: (MonadIO m, MonadReader Environment m) => String -> m FilePath
getFolderPath' s = do
    a <- ( <> "/"<> s<>"/") <$> getRootCLIFolder
    liftIO $ mkdir "-p" a
    return a

getRootCLIFolder :: (MonadIO m, MonadReader Environment m) => m FilePath
getRootCLIFolder = do
    environmentFolder <- asks (\case
                                Testnet {} -> "testnet"
                                Mainnet {} -> "mainnet")
    a <- ( <> "/.tokenomia-cli/" <> environmentFolder) <$> (liftIO . getEnv) "HOME"
    liftIO $ mkdir "-p" a
    return a

toPlutusScriptV1 :: Script.Script -> Shelley.PlutusScript Shelley.PlutusScriptV1
toPlutusScriptV1
  = Shelley.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise

getDataHash :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m String
getDataHash a = do
    filePath <- persistDataInTMP a
    liftIO $ init . C.unpack  <$>  (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

persistDataInTMP :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m FilePath
persistDataInTMP a = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let filePath = tmpFolder <> show randomInt <> ".txt"
    liftIO $ echo (dataToJSONString a) &> (Truncate . fromString) filePath
    return filePath
