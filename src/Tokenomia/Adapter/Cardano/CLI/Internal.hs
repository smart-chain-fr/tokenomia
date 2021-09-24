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
module Tokenomia.Adapter.Cardano.CLI.Internal
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
import Data.String
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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import Control.Monad.IO.Class ( MonadIO(..) )

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["cat","echo","mkdir","md5sum","mv","cardano-cli","awk","ls", "rm", "cardano-address" ]


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

   
register_shelley_wallet :: WalletName -> IO ()
register_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
        paymentSigning = walletKeyPath <> "payment-signing.skey"
        paymentVerification = walletKeyPath <> "payment-verification.vkey"
        stakeVerification = walletKeyPath <> "stake-verification.vkey"
        paymentAddress = walletKeyPath <> "payment.addr"

    mkdir "-p" walletKeyPath
    cardano_address "recovery-phrase" "generate" "--size" "24"                
        &> (Truncate . fromString) mnemonics
    (cat mnemonics |> cardano_address "key" "from-recovery-phrase" "Shelley")
        &> (Truncate . fromString) paymentSigning
    (cat paymentSigning |> cardano_address "key" "child" "1852H/1815H/0H/0/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) paymentVerification
    (cat paymentSigning |> cardano_address "key" "child" "1852H/1815H/0H/2/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) stakeVerification
    (cat paymentVerification |> cardano_address "address" "payment" "--network-tag" "testnet")
        &> (Truncate . fromString) paymentAddress
    return ()


remove_shelley_wallet :: WalletName -> IO ()
remove_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"

    rm "-rf" walletKeyPath
   

query_tip :: Cmd
query_tip = cardano_cli "query" "tip" "--testnet-magic" (1097911063::Integer)

query_utxo :: WalletAddress -> IO T.Text
query_utxo walletAddress = (TL.toStrict . TLE.decodeUtf8) <$> (cardano_cli "query" "utxo" "--testnet-magic" (1097911063::Integer) "--address" walletAddress |> capture)

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
    a <- ( <> "/"<> s<>"/") <$> getRootCLIFolder
    mkdir "-p" a
    return a

getRootCLIFolder :: IO FilePath
getRootCLIFolder = do
    a <- ( <> "/.tokenomia-cli/") <$> getEnv "HOME"
    mkdir "-p" a
    return a

toPlutusScriptV1 :: MintingPolicy -> Script.PlutusScript Script.PlutusScriptV1
toPlutusScriptV1
  = Script.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . unMintingPolicyScript
                  