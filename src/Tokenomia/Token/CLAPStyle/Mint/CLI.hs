{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Token.CLAPStyle.Mint.CLI
    ( mintI ) where
    
import           Prelude
import           Shh

import qualified Data.Text as T

import qualified Data.ByteString.UTF8 as BSU 

import           Ledger
import qualified Ledger.Value as L

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Wallet.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 


{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat","scp"]

mintI :: Wallet -> IO ()
mintI wallet@Wallet {paymentAddress = minterAddr,..} = do

    tokenNameToMint  <- echo "-n" "> Token Name : "             >>  L.tokenName . BSU.fromString <$> getLine
    amountToMint     <- echo "-n" "> Total Supply to Mint : "   >>  read @Integer <$> getLine

    echo "> Select the collateral utxo :" 
    selectUTxO wallet
        >>= \case 
            Nothing -> echo "Please, add a collateral to your wallet"
            Just utxoWithCollateral -> do 
                echo "> Select the utxo used for Minting the Token :" 
                selectUTxO wallet
                >>= \case  
                    Nothing -> echo "Please, add a collateral to your wallet"
                    Just utxoForMinting -> do 
                        let monetaryPolicy = mkMonetaryPolicyScript 
                                                Params { txOutRefToConsume = txOutRef utxoForMinting
                                                       , amount = amountToMint
                                                       , tokenName = tokenNameToMint }
                            policyhash = scriptCurrencySymbol monetaryPolicy

                        echo "-------------------------"
                        echo $ "Policy hash will be : " <> show policyhash
                        echo "-------------------------"

                        monetaryScriptFilePath <- register_minting_script_file monetaryPolicy
                        run_tx paymentSigningKeyPath
                                [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForMinting 
                                , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                , "--change-address"  , minterAddr
                                , "--mint" , show amountToMint <> " " <> show policyhash <> "." <> L.toString tokenNameToMint
                                , "--mint-script-file" , monetaryScriptFilePath
                                , "--mint-redeemer-value",  "[]"]
                                
                        echo "------------------------------------------------------"
                        echo "Done"
                        echo "------------------------------------------------------"
