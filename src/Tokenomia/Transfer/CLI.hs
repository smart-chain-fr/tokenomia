{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Transfer.CLI 
    ( run) where

import Shh
    ( load,
      (|>),
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
 
      
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat","pwd","awk","grep","mkdir"]

run :: IO ()
run = do
    echo "------------------------------------------------------"
    echo "Transferring Native Tokens from a Wallet to another "
    echo "------------------------------------------------------"
    echo "Node tip" 
    echo "---------"
    
    query_tip

    echo "---------"
    echo "Collecting inputs for transferring Native Tokens" 
    echo "---------"

 
    senderAddr      <- echo "-n" "> Sender address : "    >>  getLine
    receiverAddr    <- echo "-n" "> Receiver address : "  >>  getLine

    echo "Sender Wallet Content :" 
    __ <- print <$> getUTxOs senderAddr
    
    tokenPolicyHash <- echo "-n" "> Token policy hash : " >>  getLine
    tokenName       <- echo "-n" "> Token Name : "        >>  getLine
    amount          <- echo "-n" "> Amount of Token : "   >>  read @Int <$> getLine
    

    utxoWithToken                  <- echo "-n" "> UTxO(TxHash#TxIx) to send (containinng your tokens) :" >> getLine
    totalTokenAmountFromUtxoToken  <- echo "-n" "> Total amount in its UTxO :"                            >> read @Int <$> getLine
    utxoWithFees                   <- echo "-n" "> UTxO(TxHash#TxIx) to pay fees :"                       >> getLine 
    utxiWithCollateral             <- echo "-n" "> UTxO(TxHash#TxIx) for collateral :"                    >> getLine  
    

    privateKeyPath <- echo "-n" "> Sender Private key path : "  >> getLine 
   
    run_tx privateKeyPath 
            [ "--tx-in"  , utxoWithToken 
            , "--tx-in"  , utxoWithFees 
            , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> tokenPolicyHash <> "." <> tokenName 
            , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalTokenAmountFromUtxoToken - amount) <> " " <> tokenPolicyHash <> "." <> tokenName 
            , "--tx-in-collateral", utxiWithCollateral 
            , "--change-address"  , senderAddr]

    echo "------------------------------------------------------"
    echo "Done"
    echo "------------------------------------------------------"