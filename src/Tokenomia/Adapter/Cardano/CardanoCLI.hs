{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}

{- | It's a simple wrapper around the cardano-cli with Shh library. Waiting for the PAB to be available 
     on the Testnet and the Mainnet, Whe have a need for a complex sequences of transactions. 
     To be less error prone we have chosen this alternative over using bash scripts for example.
     It allows you to use a part of your Off chain codebase in Haskell basically. 
-}
module Tokenomia.Adapter.Cardano.CardanoCLI
    ( -- Write 
      run_tx
    , register_minting_script_file
    , register_shelley_wallet
    , remove_shelley_wallet
      -- Read 
    , query_registered_wallets  
    , query_utxo
    , query_tip
    , Wallet (..)
    , WalletAddress) where

import Shh.Internal
    ( Cmd,
      ExecArg(asArg),
      captureWords,
      load,
      (|>),
      ExecReference(SearchPath), capture )

import System.Environment (getEnv)
import qualified Data.ByteString.Lazy.Char8 as C
import Ledger.Contexts ( scriptCurrencySymbol )
import Cardano.Api ( writeFileTextEnvelope, Error(displayError) )
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise ( serialise )
import qualified Cardano.Api as Script
import qualified Cardano.Api.Shelley  as Script
import Ledger ( unMintingPolicyScript, MintingPolicy )
import Data.List.NonEmpty ( NonEmpty, fromList )
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["cat","echo","mkdir","md5sum","mv","cardano-cli","awk","ls", "rm" ]

type TxOutRef = String
type WalletAddress = String
type WalletName = String
type PaymentAddress = String

data Wallet = Wallet 
              { name :: WalletName
              , paymentAddress :: PaymentAddress
              , paymentSigningKeyPath :: FilePath } 

instance Show Wallet where 
    show Wallet {..} = ">> " <> name <> " :" <> paymentAddress 



query_registered_wallets :: IO (Maybe (NonEmpty Wallet))
query_registered_wallets = do 
   keyPath <- getFolderPath Keys
   walletNames <- (fmap.fmap) C.unpack (ls keyPath |> captureWords)
   mapM (\name -> 
        do 
        let paymentAddressPath = keyPath <> name <> "/payment.addr"  
            paymentSigningKeyPath = keyPath <> name <> "/payment-signing.skey"  
        paymentAddress <- C.unpack  <$> (cat paymentAddressPath |> capture ) 
        return $ Wallet {..} ) walletNames
    >>= \case 
            [] -> return Nothing 
            a  -> (return . Just . fromList) a   


remove_shelley_wallet :: WalletName -> IO ()
remove_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"

    rm "-rf" walletKeyPath
   
   

register_shelley_wallet :: WalletName -> IO ()
register_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        paymentSigning = walletKeyPath <> "payment-signing.skey"
        paymentVerification = walletKeyPath <> "payment-verification.vkey"
        stakeSigning = walletKeyPath <> "stake-signing.skey"
        stakeVerification = walletKeyPath <> "stake-verification.vkey"
        paymentAddress = walletKeyPath <> "payment.addr"

    mkdir "-p" walletKeyPath
    cardano_cli "address" "key-gen"
        "--verification-key-file" paymentVerification
        "--signing-key-file" paymentSigning
    cardano_cli "stake-address" "key-gen" 
        "--verification-key-file" stakeVerification 
        "--signing-key-file" stakeSigning
    cardano_cli "address" "build" 
        "--payment-verification-key-file" paymentVerification 
        "--stake-verification-key-file" stakeVerification 
        "--out-file" paymentAddress
        "--testnet-magic" (1097911063::Integer)

query_tip :: Cmd
query_tip = cardano_cli "query" "tip" "--testnet-magic" (1097911063::Integer)

query_utxo :: WalletAddress -> Cmd
query_utxo = cardano_cli "query" "utxo" "--testnet-magic" (1097911063::Integer) "--address"

-- | Build a Tx , Sign it with the private key path provided and Submit it
--   Temporary Files are persisted into ~/.cardano-cli/ folder 
run_tx :: ExecArg a => FilePath -> a  -> IO ()
run_tx privateKeyPath buildTxBody = do

    (txFolder, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions
    protocolParametersPath <- register_protocol_parameters
    cardano_cli
        "transaction"
        "build"
        "--alonzo-era"
        "--testnet-magic" (1097911063::Integer)
        (asArg buildTxBody)
        "--protocol-params-file" protocolParametersPath
        "--out-file" rawTx

    -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . head <$> (md5sum rawTx |> captureWords )
    mv rawTx rawHashTx

    echo "Signing Tx"    >> sign_tx   rawHashTx signedHashTx privateKeyPath
    echo "Submitting Tx" >> submit_tx signedHashTx
    echo "Tx sent"

submit_tx :: FilePath ->  Cmd
submit_tx = cardano_cli "transaction" "submit"
                        "--testnet-magic" (1097911063::Integer)
                        "--tx-file"

sign_tx :: FilePath -> FilePath -> FilePath  -> Cmd
sign_tx body_file outFile signing_key_file
    = cardano_cli "transaction" "sign"
            "--tx-body-file" body_file
            "--signing-key-file" signing_key_file
            "--testnet-magic" (1097911063::Integer)
            "--out-file" outFile

register_protocol_parameters :: IO FilePath
register_protocol_parameters = do
    folder <- getFolderPath Parameters
    let filePath =  folder <> "parameters-testnet.json"
    cardano_cli
        "query"
        "protocol-parameters"
        "--testnet-magic" (1097911063::Integer)
        "--out-file" filePath
    return filePath

register_minting_script_file :: MintingPolicy -> IO FilePath
register_minting_script_file mp = do
    txFolder <- getFolderPath Transactions
    let filePath =  txFolder <> show (scriptCurrencySymbol mp) <> ".plutus"
    writeFileTextEnvelope filePath Nothing (toPlutusScriptV1 mp)
        >>= (\case
            Left err -> error $ displayError err
            Right () -> return filePath)


data Folder = Transactions | Keys | Parameters

getFolderPath :: Folder -> IO FilePath 
getFolderPath folder 
    =  getFolderPath' 
            $ case folder of 
                Transactions -> "transactions"
                Keys ->  "keys"
                Parameters -> "parameters"   
  
getFolderPath' ::  String -> IO FilePath
getFolderPath' s = do 
    a <- ( <> "/"<> s<>"/") <$> getCardanoCLIFolder
    mkdir "-p" a
    return a

getCardanoCLIFolder :: IO FilePath
getCardanoCLIFolder = do
    a <- ( <> "/.cardano-cli/") <$> getEnv "HOME"
    mkdir "-p" a
    return a

toPlutusScriptV1 :: MintingPolicy -> Script.PlutusScript Script.PlutusScriptV1
toPlutusScriptV1
  = Script.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . unMintingPolicyScript
                  