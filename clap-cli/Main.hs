{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Shh
import Data.ByteString.Lazy.Char8 (unpack)

load SearchPath ["echo","ssh","cat"]

main :: IO ()
main = do
    echo "Send CLAP"
    echo "Cardano Node Sync Status"
    execCardanoCLICommand "query tip --testnet-magic 1097911063"
    echo "Available UTxOs at wallet address"
    execCardanoCLICommand "query utxo --testnet-magic 1097911063 --address \\$(cat \\$CNODE_HOME/custom/charles/address)"

    echo "Enter the receiver address"
    receiverAddress <- getLine
    echo "Enter amount of CLAPs (in mCLAP)"
    clapAmountToSend <- getLine
    echo "Enter UTxO containing CLAPs to send (TxHash#TxIx)"
    utxoCLAP <- getLine
    echo "Enter UTxO to pay fees (TxHash#TxIx)"
    utxoFees <- getLine 
    echo "Enter UTxO for collateral (TxHash#TxIx)"
    utxoCollateral <- getLine
    echo "Building Tx"
    clapIn <- captureTrim <| execCardanoCLICommand (
                                "query utxo --tx-in "
                                ++ utxoCLAP
                                ++ " --testnet-magic 1097911063 | awk '{print \\$6}' | grep '\\S'"
                                )
    let clapAmountToSendInt = (read clapAmountToSend :: Int)
    let clapString = unpack clapIn
    let clapInInt = (read clapString :: Int)
    let clapChange = clapInInt - clapAmountToSendInt
    let clapChangeString = (show clapChange :: [Char])
    cardanoCLIBuildTx receiverAddress clapAmountToSend utxoCLAP clapChangeString utxoFees utxoCollateral
    echo "Signing Tx"
    cardanoCLISignTx
    echo "Sending Tx"
    cardanoCLISubmitTx
    echo "done"


cardanoCLISubmitTx :: Cmd 
cardanoCLISubmitTx = execCardanoCLICommand (
                        "transaction submit --testnet-magic 1097911063 "
                        ++ "--tx-file /opt/cardano/cnode/custom/charles/tx.signed"
                        )

cardanoCLISignTx :: Cmd 
cardanoCLISignTx = execCardanoCLICommand (
                    "transaction sign --tx-body-file /opt/cardano/cnode/custom/charles/tx.raw "
                    ++ "--signing-key-file /opt/cardano/cnode/custom/charles/privkey "
                    ++ "--testnet-magic 1097911063 "
                    ++ "--out-file /opt/cardano/cnode/custom/charles/tx.signed"
                    )

cardanoCLIBuildTx :: String -> String -> String -> String -> String -> String -> Cmd
cardanoCLIBuildTx receiver amount utxoClap clapChange fees collateral
    = execCardanoCLICommand (
        "transaction build --alonzo-era --testnet-magic 1097911063 --tx-in "
        ++ utxoClap
        ++ " --tx-in "
        ++ fees
        ++ " --tx-out '"
        ++ receiver
        ++ " + 1344798 lovelace + "
        ++ amount
        ++ " bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP'"
        ++ " --tx-out 'addr_test1vpf3wtmj4mfry4cth7ulvcfjklmc7cpp7ls07z7q8cr2t3cj7999h + 1344798 lovelace + "
        ++ clapChange
        ++ " bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP' "
        ++ " --tx-in-collateral "
        ++ collateral
        ++ " --change-address addr_test1vpf3wtmj4mfry4cth7ulvcfjklmc7cpp7ls07z7q8cr2t3cj7999h"
        ++ " --out-file /opt/cardano/cnode/custom/charles/tx.raw"
        )


execCardanoCLICommand ::  String -> Cmd 
execCardanoCLICommand cardanoCLICommandBody
    =  ssh
        "-i"
        "/home/charles/.ssh/ssh_key"
        "bastion@51.158.79.20"
        arg1
    where
        arg1 :: String
        arg1  = "export KUBECONFIG=~/kubeconfig-testnet.yml"
            ++ " && kubectl exec -n cardano-testnet deploy-smartnode-cardano-testnet-relay-6d8c586944-zhsnr "
            ++ "-- bash -c \"/home/cardano/.cabal/bin/cardano-cli "
            ++ cardanoCLICommandBody ++ "\""