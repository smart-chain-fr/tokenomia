{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Smartchain.Scripts.CLAP.Transfer 
    ( runWithUserInteraction
    , run) where

import Shh
    ( (<|),
      capture,
      captureTrim,
      load,
      (|>),
      Cmd,
      ExecReference(SearchPath),
      Shell )
import Data.ByteString.Lazy.Char8 (unpack)
import Smartchain.Scripts.CardanoCLI.Wrapper
    ( executeOverBastion,
      cardano_cli,
      submit_tx,
      sign_tx,
      testnet_magic_opt,
      WalletAddress,
      TxOutRef,
      query_utxo,
      query_tip ) 
      
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat"]

runWithUserInteraction :: IO ()
runWithUserInteraction = do
    echo "------------------------------------------------------"
    echo "Interactive CLI : Transfer CLAP from a Wallet to another "
    echo "------------------------------------------------------"
    echo "Node tip" 
    echo "---------"

    query_tip

    echo "---------"
    echo "Collecting inputs for transferring CLAPs" 
    echo "---------"

    echo "Getting Sender Address at $CNODE_HOME/custom/charles/address over Bastion " 
    senderAddr <- unpack <$> (executeOverBastion "cat \\$CNODE_HOME/custom/charles/address" |> capture)

    receiverAddr  <- echo "> Enter the receiver address" >> getLine
    amount        <- echo "> Enter amount of CLAPs (in mCLAP)" >>  read @Int <$> getLine
    
    echo "Available Sender UtxO :" |> query_utxo senderAddr

    clapInTxOutRef         <- echo "> Enter the UTxOutRef containing CLAPs to send (TxHash#TxIx)" >> getLine
    feesTxOutRef           <- echo "> Enter the UTxOutRef to pay fees (TxHash#TxIx)" >> getLine 
    collateralTxOutRef     <- echo "> Enter the UTxOutRef for collateral (TxHash#TxIx)" >> getLine  
    
    echo "Building Tx"
    clapAmountFromUtxoSourceWallet :: Int 
        <- read @Int. unpack 
            <$> captureTrim 
                <| cardano_cli ("query utxo --tx-in "
                                <> clapInTxOutRef <> " "
                                <> testnet_magic_opt <> " | awk '{print \\$6}' | grep '\\S'")
    
    run 
        senderAddr
        receiverAddr 
        clapInTxOutRef 
        feesTxOutRef 
        collateralTxOutRef
        amount 
        (clapAmountFromUtxoSourceWallet - amount) 
    
    echo "done"


run
    :: Shell f 
    => WalletAddress
    -> WalletAddress 
    -> TxOutRef 
    -> TxOutRef 
    -> TxOutRef 
    -> Int 
    -> Int 
    -> f ()
run a b c d e f g = build_tx a b c d e f g |> sign_tx |>  submit_tx

build_tx 
    :: WalletAddress
    -> WalletAddress 
    -> TxOutRef 
    -> TxOutRef 
    -> TxOutRef 
    -> Int 
    -> Int 
    -> Cmd
build_tx 
    senderAddr
    receiverAddr 
    clapInTxOutRef 
    feesTxOutRef 
    collateralTxOutRef
    amount 
    clapChange 
    = cardano_cli (
        "transaction build " 
            <> " --alonzo-era "
            <> " --testnet-magic 1097911063 " 
            <> "--tx-in " <> clapInTxOutRef <> " "
            <> "--tx-in " <> feesTxOutRef <> " "
            <> "--tx-out '" <> receiverAddr <> " + 1344798 lovelace + " <> show amount     <> " bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP' "
            <> "--tx-out '" <> senderAddr   <> " + 1344798 lovelace + " <> show clapChange <> " bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP' "
            <> "--tx-in-collateral " <> collateralTxOutRef <> " "
            <> "--change-address " <> senderAddr <> " "
            <> "--out-file /opt/cardano/cnode/custom/charles/tx.raw")


