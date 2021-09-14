{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Smartchain.Scripts.CardanoCLI.Wrapper 
    ( cardano_cli
    , submit_tx
    , sign_tx
    , query_utxo
    , query_tip
    , testnet_magic_opt
    , executeOverBastion
    , TxOutRef
    , WalletAddress) where

import Shh ( load, (|>), Cmd, ExecReference(SearchPath), Shell )

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["ssh","cat","echo"]

type TxOutRef = String 
type WalletAddress = String

executeOverBastion ::  String -> Cmd
executeOverBastion cmd 
    =  ssh
        "-i"
        "/home/charles/.ssh/ssh_key"
        "bastion@51.158.79.20"
        arg1
    where
        arg1 :: String
        arg1  = "export KUBECONFIG=~/kubeconfig-testnet.yml"
            ++ " && kubectl exec -n cardano-testnet deploy-smartnode-cardano-testnet-relay-6d8c586944-zhsnr "
            ++ "-- bash -c \" " <> cmd <> "\""    

query_tip :: Cmd 
query_tip = cardano_cli $ "query tip " <> testnet_magic_opt

query_utxo :: WalletAddress -> Cmd 
query_utxo address
    = cardano_cli 
        $ "query utxo "
             <> testnet_magic_opt 
             <> " --address " <> address 

cardano_cli ::  String -> Cmd 
cardano_cli body =  executeOverBastion $ "/home/cardano/.cabal/bin/cardano-cli " ++ body 

submit_tx :: Shell f => f () 
submit_tx 
    = echo "Sending Tx" 
        |> cardano_cli 
           ("transaction submit " 
           <> testnet_magic_opt
           <> "--tx-file /opt/cardano/cnode/custom/charles/tx.signed")

sign_tx :: Shell f => f () 
sign_tx 
    = echo "Sending Tx" 
        |> cardano_cli 
           ("transaction sign " 
            <> "--tx-body-file /opt/cardano/cnode/custom/charles/tx.raw "
            <> "--signing-key-file /opt/cardano/cnode/custom/charles/privkey "
            <> testnet_magic_opt
            <> "--out-file /opt/cardano/cnode/custom/charles/tx.signed")
                    


testnet_magic_opt :: String 
testnet_magic_opt = "--testnet-magic 1097911063 "
                        