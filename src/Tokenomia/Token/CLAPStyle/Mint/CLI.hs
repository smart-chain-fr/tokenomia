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

import           Data.Aeson
import qualified Data.Maybe as M
import           Data.List.Split
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU 

import           Ledger
import qualified Ledger.Value as L

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CardanoCLI


{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat","scp"]

mintI :: Wallet -> IO ()
mintI Wallet {paymentAddress = minterAddr,..} = do

    tokenName       <- echo "-n" "> Token Name : "             >>  L.tokenName . BSU.fromString <$> getLine
    amount          <- echo "-n" "> Total Supply to Mint : "   >>  read @Integer <$> getLine

    echo "-------------------------"
    query_utxo minterAddr
    echo "-------------------------"

    utxiWithCollateral       <- echo "-n" "> UTxO(TxHash#TxIx) for collateral :" >> getLine
    (txIdGiven,txIndexGiven) <- echo "-n" "> UTxO(TxHash#TxIx) used for creating the Token :"
                        >> (\case
                                [a,b] -> (a, read @Integer b)
                                _ -> error "unexpected input") . splitOn "#" <$> getLine

    let txOutRef = TxOutRef ((M.fromJust . decode . BLU.fromString )  ("{\"getTxId\" : \"" <> txIdGiven <> "\"}")) txIndexGiven
        monetaryPolicy = mkMonetaryPolicyScript Params {..}
        policyhash = scriptCurrencySymbol monetaryPolicy

    echo "-------------------------"
    echo $ "Policy hash will be : " <> show policyhash
    echo "-------------------------"

    monetaryScriptFilePath <- register_minting_script_file monetaryPolicy
    run_tx paymentSigningKeyPath
            [ "--tx-in"  , txIdGiven <> "#" <>  show txIndexGiven
            , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show policyhash <> "." <> L.toString tokenName
            , "--tx-in-collateral", utxiWithCollateral
            , "--change-address"  , minterAddr
            , "--mint" , show amount <> " " <> show policyhash <> "." <> L.toString tokenName
            , "--mint-script-file" , monetaryScriptFilePath
            , "--mint-redeemer-value",  "[]"]
            
    echo "------------------------------------------------------"
    echo "Done"
    echo "------------------------------------------------------"
