{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Transfer.CLI 
    ( run) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
import Control.Monad.Catch ( MonadMask ) 
      
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat","pwd","awk","grep","mkdir"]

run :: (MonadMask m, MonadIO m, MonadReader Environment m)  => m ()
run = do
    liftIO $ echo "------------------------------------------------------"
    liftIO $ echo "Transferring Native Tokens from a Wallet to another "
    liftIO $ echo "------------------------------------------------------"
    liftIO $ echo "Node tip" 
    liftIO $ echo "---------"
    
    output <- query_tip
    liftIO $ print output

    liftIO $ echo "---------"
    liftIO $ echo "Collecting inputs for transferring Native Tokens" 
    liftIO $ echo "---------"

 
    senderAddr      <- liftIO $ echo "-n" "> Sender address : "    >>  getLine
    receiverAddr    <- liftIO $ echo "-n" "> Receiver address : "  >>  getLine

    liftIO $ echo "Sender Wallet Content :" 
    __ <- print <$> getUTxOs senderAddr
    
    tokenPolicyHash <- liftIO $ echo "-n" "> Token policy hash : " >>  getLine
    tokenName       <- liftIO $ echo "-n" "> Token Name : "        >>  getLine
    amount          <- liftIO $ echo "-n" "> Amount of Token : "   >>  read @Int <$> getLine
    

    utxoWithToken                  <- liftIO $ echo "-n" "> UTxO(TxHash#TxIx) to send (containinng your tokens) :" >> getLine
    totalTokenAmountFromUtxoToken  <- liftIO $ echo "-n" "> Total amount in its UTxO :"                            >> read @Int <$> getLine
    utxoWithFees                   <- liftIO $ echo "-n" "> UTxO(TxHash#TxIx) to pay fees :"                       >> getLine 
    utxiWithCollateral             <- liftIO $ echo "-n" "> UTxO(TxHash#TxIx) for collateral :"                    >> getLine  
    

    privateKeyPath <- liftIO $ echo "-n" "> Sender Private key path : "  >> getLine 
   
    run_tx privateKeyPath 
            [ "--tx-in"  , utxoWithToken 
            , "--tx-in"  , utxoWithFees 
            , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> tokenPolicyHash <> "." <> tokenName 
            , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalTokenAmountFromUtxoToken - amount) <> " " <> tokenPolicyHash <> "." <> tokenName 
            , "--tx-in-collateral", utxiWithCollateral 
            , "--change-address"  , senderAddr]

    liftIO $ echo "------------------------------------------------------"
    liftIO $ echo "Done"
    liftIO $ echo "------------------------------------------------------"